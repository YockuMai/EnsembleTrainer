# R/preprocess_functions.R
# Функциональный аналог PreprocessData (без R6)

#' Вспомогательная: проверка, все ли значения NA
.safe_all_na <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  all(is.na(x))
}

#' Вспомогательная: маска пропусков для одного вектора
.get_missing_mask_single <- function(x) {
  if (is.numeric(x)) {
    return(is.na(x))
  } else {
    ch <- as.character(x)
    return(is.na(ch) | ch == "" | ch %in% c("N/A", "NULL", "NA", "null", "n/a"))
  }
}

#' Определение типов колонок
detect_columns <- function(data) {
  list(
    numeric = names(data)[vapply(data, function(col) is.numeric(col) || is.integer(col), logical(1))],
    factor  = names(data)[vapply(data, is.factor, logical(1))],
    other   = setdiff(names(data), c(
      names(data)[vapply(data, function(col) is.numeric(col) || is.integer(col), logical(1))],
      names(data)[vapply(data, is.factor, logical(1))]
    ))
  )
}

#' Вычисление статистик для числовых колонок
compute_numeric_stats <- function(data, cols) {
  if (length(cols) == 0) return(list())
  stats <- list()
  for (cn in cols) {
    vec <- data[[cn]]
    if (.safe_all_na(vec)) {
      stats$min[[cn]] <- NA_real_
      stats$max[[cn]] <- NA_real_
      stats$mean[[cn]] <- NA_real_
      stats$Q1[[cn]]  <- NA_real_
      stats$Q3[[cn]]  <- NA_real_
    } else {
      stats$min[[cn]]  <- min(vec, na.rm = TRUE)
      stats$max[[cn]]  <- max(vec, na.rm = TRUE)
      stats$mean[[cn]] <- mean(vec, na.rm = TRUE)
      stats$Q1[[cn]]   <- as.numeric(quantile(vec, probs = 0.25, na.rm = TRUE, type = 7))
      stats$Q3[[cn]]   <- as.numeric(quantile(vec, probs = 0.75, na.rm = TRUE, type = 7))
    }
  }
  stats
}

#' Вычисление статистик для факторных колонок (мода)
compute_factor_stats <- function(data, cols) {
  if (length(cols) == 0) return(list())
  stats <- list(moda = list())
  for (cn in cols) {
    col <- data[[cn]]
    charv <- as.character(col)
    charv <- charv[!is.na(charv)]
    if (length(charv) == 0) {
      stats$moda[[cn]] <- NA_character_
    } else {
      ux <- unique(charv)
      stats$moda[[cn]] <- ux[which.max(tabulate(match(charv, ux)))]
    }
  }
  stats
}

#' Получение статистики о пропусках
get_missing_statistic <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(list(rows = 0, count = 0, percentage = 0))
  }
  masks <- vapply(data, .get_missing_mask_single, logical(nrow(data)))
  if (is.vector(masks)) masks <- matrix(masks, ncol = 1)
  rows_with_missing <- rowSums(masks, na.rm = TRUE) > 0
  total_rows <- nrow(data)
  count <- sum(rows_with_missing)
  percentage <- if (total_rows > 0) round((count / total_rows) * 100, 2) else 0
  list(rows = total_rows, count = count, percentage = percentage)
}

#' Заполнение / удаление пропусков
clear_missing <- function(data, method = "mean") {
  if (is.null(data) || nrow(data) == 0) return(data)
  
  if (method == "delete") {
    masks <- vapply(data, .get_missing_mask_single, logical(nrow(data)))
    if (is.vector(masks)) masks <- matrix(masks, ncol = 1)
    rows_with_missing <- rowSums(masks, na.rm = TRUE) > 0
    data <- data[!rows_with_missing, , drop = FALSE]
    return(data)
  } 
  else if (method == "mean") {
    col_types <- detect_columns(data)
    # Числовые -> mean
    for (cn in col_types$numeric) {
      vec <- data[[cn]]
      mask <- .get_missing_mask_single(vec)
      if (any(mask, na.rm = TRUE)) {
        mean_val <- mean(vec, na.rm = TRUE)
        if (!is.na(mean_val)) data[[cn]][mask] <- mean_val
      }
    }
    # Факторы -> мода
    for (cn in col_types$factor) {
      vec <- data[[cn]]
      mask <- .get_missing_mask_single(vec)
      if (any(mask, na.rm = TRUE)) {
        charv <- as.character(vec)
        charv <- charv[!is.na(charv)]
        if (length(charv) > 0) {
          ux <- unique(charv)
          mode_val <- ux[which.max(tabulate(match(charv, ux)))]
          if (is.factor(data[[cn]])) {
            if (!(mode_val %in% levels(data[[cn]]))) {
              levels(data[[cn]]) <- c(levels(data[[cn]]), mode_val)
            }
            data[[cn]][mask] <- mode_val
          } else {
            data[[cn]][mask] <- mode_val
          }
        }
      }
    }
    return(data)
  } 
  else {
    stop("clear_missing: method must be 'delete' or 'mean'")
  }
}

#' Статистика выбросов
get_outliers_statistic <- function(data, iqr_multiplier = 1.5) {
  col_types <- detect_columns(data)
  numeric_cols <- col_types$numeric
  if (length(numeric_cols) == 0) {
    return(list(total_outliers = 0, numeric_columns = character(0), outliers_by_column = list()))
  }
  
  # Вычислим Q1 и Q3 для всех числовых колонок
  stats <- compute_numeric_stats(data, numeric_cols)
  outliers_by_column <- list()
  total_outliers <- 0
  
  for (cn in numeric_cols) {
    vec <- data[[cn]]
    q1 <- stats$Q1[[cn]]
    q3 <- stats$Q3[[cn]]
    if (is.null(q1) || is.null(q3) || is.na(q1) || is.na(q3)) next
    iqr <- q3 - q1
    lower <- q1 - iqr_multiplier * iqr
    upper <- q3 + iqr_multiplier * iqr
    mask <- (!is.na(vec)) & (vec < lower | vec > upper)
    cnt <- sum(mask, na.rm = TRUE)
    if (cnt > 0) {
      pct <- round((cnt / length(vec)) * 100, 2)
      outliers_by_column[[cn]] <- list(count = cnt, percentage = pct, lower = lower, upper = upper)
      total_outliers <- total_outliers + cnt
    }
  }
  list(total_outliers = total_outliers, numeric_columns = numeric_cols, outliers_by_column = outliers_by_column)
}

#' Удаление или ограничение выбросов
clear_outliers <- function(data, method = "replace", iqr_multiplier = 1.5) {
  col_types <- detect_columns(data)
  numeric_cols <- col_types$numeric
  if (length(numeric_cols) == 0) return(data)
  
  stats <- compute_numeric_stats(data, numeric_cols)
  
  if (method == "delete") {
    n <- nrow(data)
    rows_to_remove <- rep(FALSE, n)
    for (cn in numeric_cols) {
      vec <- data[[cn]]
      q1 <- stats$Q1[[cn]]
      q3 <- stats$Q3[[cn]]
      if (is.null(q1) || is.null(q3) || is.na(q1) || is.na(q3)) next
      iqr <- q3 - q1
      lower <- q1 - iqr_multiplier * iqr
      upper <- q3 + iqr_multiplier * iqr
      mask <- (!is.na(vec)) & (vec < lower | vec > upper)
      rows_to_remove <- rows_to_remove | mask
    }
    if (any(rows_to_remove)) {
      data <- data[!rows_to_remove, , drop = FALSE]
    }
    return(data)
  } 
  else if (method == "replace") {
    for (cn in numeric_cols) {
      vec <- data[[cn]]
      q1 <- stats$Q1[[cn]]
      q3 <- stats$Q3[[cn]]
      if (is.null(q1) || is.null(q3) || is.na(q1) || is.na(q3)) next
      iqr <- q3 - q1
      lower <- q1 - iqr_multiplier * iqr
      upper <- q3 + iqr_multiplier * iqr
      data[[cn]] <- pmin(pmax(vec, lower), upper)
    }
    return(data)
  } 
  else {
    stop("clear_outliers: method must be 'delete' or 'replace'")
  }
}

#' Нормализация (min-max). Возвращает список с данными и параметрами для обратного преобразования.
normalize <- function(data) {
  col_types <- detect_columns(data)
  numeric_cols <- col_types$numeric
  if (length(numeric_cols) == 0) return(list(data = data, params = list()))
  
  stats <- compute_numeric_stats(data, numeric_cols)
  mins <- stats$min
  maxs <- stats$max
  new_data <- data
  for (cn in numeric_cols) {
    minv <- mins[[cn]]
    maxv <- maxs[[cn]]
    if (is.na(minv) || is.na(maxv) || maxv == minv) {
      new_data[[cn]] <- ifelse(is.na(data[[cn]]), NA_real_, 0)
    } else {
      new_data[[cn]] <- (data[[cn]] - minv) / (maxv - minv)
    }
  }
  list(data = new_data, params = list(min = mins, max = maxs, type = "normalized"))
}

#' Стандартизация (z-score). Возвращает список с данными и параметрами.
standardize <- function(data) {
  col_types <- detect_columns(data)
  numeric_cols <- col_types$numeric
  if (length(numeric_cols) == 0) return(list(data = data, params = list()))
  
  means <- list()
  sds <- list()
  new_data <- data
  for (cn in numeric_cols) {
    vec <- data[[cn]]
    if (.safe_all_na(vec)) {
      means[[cn]] <- NA_real_
      sds[[cn]] <- NA_real_
      new_data[[cn]] <- ifelse(is.na(vec), NA_real_, 0)
    } else {
      m <- mean(vec, na.rm = TRUE)
      s <- sd(vec, na.rm = TRUE)
      means[[cn]] <- m
      sds[[cn]] <- s
      if (is.na(s) || s == 0) {
        new_data[[cn]] <- ifelse(is.na(vec), NA_real_, 0)
      } else {
        new_data[[cn]] <- (vec - m) / s
      }
    }
  }
  list(data = new_data, params = list(mean = means, sd = sds, type = "standardized"))
}

#' Обратное преобразование (денормализация / дестандартизация)
denormalize_or_destandardize <- function(data, params) {
  if (is.null(params) || length(params) == 0) return(data)
  type <- params$type
  if (is.null(type)) return(data)
  
  col_types <- detect_columns(data)
  numeric_cols <- col_types$numeric
  new_data <- data
  
  if (type == "normalized") {
    mins <- params$min
    maxs <- params$max
    for (cn in numeric_cols) {
      minv <- mins[[cn]]
      maxv <- maxs[[cn]]
      if (!is.null(minv) && !is.null(maxv) && !is.na(minv) && !is.na(maxv) && maxv > minv) {
        new_data[[cn]] <- data[[cn]] * (maxv - minv) + minv
      }
    }
  } 
  else if (type == "standardized") {
    means <- params$mean
    sds <- params$sd
    for (cn in numeric_cols) {
      m <- means[[cn]]
      s <- sds[[cn]]
      if (!is.null(m) && !is.null(s) && !is.na(m) && !is.na(s) && s > 0) {
        new_data[[cn]] <- data[[cn]] * s + m
      }
    }
  }
  new_data
}

#' Переименование колонок
rename_columns <- function(data, name_mapping) {
  # name_mapping: именованный вектор c(старое = "новое", ...)
  if (is.null(name_mapping) || length(name_mapping) == 0) return(data)
  for (old in names(name_mapping)) {
    new <- name_mapping[[old]]
    if (old %in% names(data) && new != old && nzchar(new)) {
      names(data)[names(data) == old] <- new
    }
  }
  data
}

#' Удаление колонок
remove_columns <- function(data, columns_to_remove) {
  if (is.null(columns_to_remove) || length(columns_to_remove) == 0) return(data)
  cols_keep <- setdiff(names(data), columns_to_remove)
  data[, cols_keep, drop = FALSE]
}

#' Преобразовать указанные колонки в факторы
set_factor_columns <- function(data, columns) {
  if (is.null(columns) || length(columns) == 0) return(data)
  for (col in intersect(columns, names(data))) {
    data[[col]] <- as.factor(data[[col]])
  }
  data
}

#' Преобразовать указанные колонки в числовые
set_numeric_columns <- function(data, columns) {
  if (is.null(columns) || length(columns) == 0) return(data)
  for (col in intersect(columns, names(data))) {
    data[[col]] <- as.numeric(as.character(data[[col]]))
  }
  data
}

#' Переименование с проверками (аналог set_columns_name)
set_columns_name <- function(data, rename_vector) {
  if (!is.character(rename_vector) || is.null(names(rename_vector))) {
    stop("rename_vector must be a named character vector")
  }
  current_names <- colnames(data)
  if (!all(names(rename_vector) %in% current_names)) {
    stop("Some columns to rename not found")
  }
  new_names <- current_names
  idx <- match(names(rename_vector), current_names)
  new_names[idx] <- rename_vector
  if (any(is.na(new_names)) || any(trimws(new_names) == "")) {
    stop("Column names cannot be empty")
  }
  if (any(duplicated(new_names))) {
    stop("Duplicate column names after renaming")
  }
  colnames(data) <- new_names
  data
}