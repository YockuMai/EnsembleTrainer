PreprocessData <- R6::R6Class("PreprocessData",
  private = list(
    data = NULL,

    # типы колонок (всегда актуальные после вызова detect_columns)
    numeric_cols = character(0),
    factor_cols  = character(0),
    no_type_cols = character(0),

    # статистики храним как именованные списки: stats$min[[col]], stats$moda[[col]] и т.д.
    stats = list(
      min = list(),
      max = list(),
      Q1  = list(),
      Q3  = list(),
      mean = list(),
      moda = list()
    ),

    scaling_type = "none",         # "none", "normalized", "standardized"
    original_params = list(),      # для denormalize/standardize: min/max или mean/sd

    ### ---------- HELPERS ----------

    safe_all_na = function(x) {
      # TRUE если все значения NA (или длина 0)
      if (is.factor(x)) x <- as.character(x)
      all(is.na(x))
    },

    detect_columns = function() {
      cols <- names(private$data)
      private$numeric_cols <- cols[vapply(private$data, function(col) is.numeric(col) || is.integer(col), logical(1))]
      private$factor_cols  <- cols[vapply(private$data, is.factor, logical(1))]
      private$no_type_cols <- setdiff(cols, c(private$numeric_cols, private$factor_cols))
    },

    compute_numeric_stats = function(cols = private$numeric_cols) {
      if (length(cols) == 0) return(invisible(NULL))

      # min / max / mean / Q1 / Q3 — vapply по колонкам (векторно по колонкам)
      mins <- setNames(vapply(cols, function(cn) {
        vec <- private$data[[cn]]
        if (private$safe_all_na(vec)) NA_real_ else min(vec, na.rm = TRUE)
      }, numeric(1)), cols)

      maxs <- setNames(vapply(cols, function(cn) {
        vec <- private$data[[cn]]
        if (private$safe_all_na(vec)) NA_real_ else max(vec, na.rm = TRUE)
      }, numeric(1)), cols)

      means <- setNames(vapply(cols, function(cn) {
        vec <- private$data[[cn]]
        if (private$safe_all_na(vec)) NA_real_ else mean(vec, na.rm = TRUE)
      }, numeric(1)), cols)

      Q1s <- setNames(vapply(cols, function(cn) {
        vec <- private$data[[cn]]
        if (private$safe_all_na(vec)) NA_real_ else as.numeric(quantile(vec, probs = 0.25, na.rm = TRUE, type = 7))
      }, numeric(1)), cols)

      Q3s <- setNames(vapply(cols, function(cn) {
        vec <- private$data[[cn]]
        if (private$safe_all_na(vec)) NA_real_ else as.numeric(quantile(vec, probs = 0.75, na.rm = TRUE, type = 7))
      }, numeric(1)), cols)

      # сохраняем в списки (замена/дополнение только по cols)
      private$stats$min[cols] <- as.list(mins)
      private$stats$max[cols] <- as.list(maxs)
      private$stats$mean[cols] <- as.list(means)
      private$stats$Q1[cols]  <- as.list(Q1s)
      private$stats$Q3[cols]  <- as.list(Q3s)
    },

    compute_factor_stats = function(cols = private$factor_cols) {
      if (length(cols) == 0) return(invisible(NULL))

      modas <- setNames(vapply(cols, function(cn) {
        col <- private$data[[cn]]
        # работаем с вектором значений без NA
        charv <- as.character(col)
        charv <- charv[!is.na(charv)]
        if (length(charv) == 0) return(NA_character_)
        ux <- unique(charv)
        ux[which.max(tabulate(match(charv, ux)))]
      }, character(1)), cols)

      private$stats$moda[cols] <- as.list(modas)
    },

    calculate_stats = function(cols = NULL) {
      # Если cols == NULL — пересчитываем всё
      if (is.null(cols)) {
        private$compute_numeric_stats(private$numeric_cols)
        private$compute_factor_stats(private$factor_cols)
      } else {
        # пересчитываем только указанные колонки (поддерживаются частично numeric/factor)
        numeric_cols <- intersect(cols, private$numeric_cols)
        factor_cols  <- intersect(cols, private$factor_cols)
        if (length(numeric_cols) > 0) private$compute_numeric_stats(numeric_cols)
        if (length(factor_cols)  > 0) private$compute_factor_stats(factor_cols)
      }
    },

    get_missing_mask_single = function(col) {
      # вернёт логический вектор длины nrow(private$data)
      if (is.numeric(col) || is.integer(col)) {
        return(is.na(col))
      } else {
        # фактор/character/прочее — считаем пустые строки и варианты "NA"/"N/A" и т.п. за пропуск
        # сначала привести к character (факторы — безопасно)
        ch <- as.character(col)
        return(is.na(ch) | ch == "" | ch %in% c("N/A", "NULL", "NA", "null", "n/a"))
      }
    },

    # вспомогательная функция: переименовать ключи в списках статистик/ориг. параметрах
    rename_in_named_list = function(lst, old, new) {
      if (length(lst) == 0) return(lst)
      # если ключ old есть, перенести в new
      if (!is.null(lst[[old]])) {
        lst[[new]] <- lst[[old]]
        lst[[old]] <- NULL
      }
      lst
    }
  ),

  public = list(
    ### ---------- Инициализация ----------
    initialize = function(data) {
      if (!is.data.frame(data)) stop("PreprocessData: data должен быть data.frame")
      private$data <- data
      private$detect_columns()
      private$calculate_stats()   # полный пересчет при инициализации
    },

    ### ---------- Доступ к данным и мета ----------
    get_data = function() {
      private$data
    },

    get_summary = function() {
      # вернёт стандартный summary() — можно оставить как есть
      summary(private$data)
    },

    get_numeric_columns = function() private$numeric_cols,
    get_factor_columns  = function() private$factor_cols,
    get_no_type_columns = function() private$no_type_cols,

    get_scaling_type = function() private$scaling_type,

    get_statistic = function() private$stats,

    get_missing_statistic = function() {
      if (is.null(private$data) || nrow(private$data) == 0) {
        return(list(rows = 0, count = 0, percentage = 0))
      }

      # Собираем маски пропусков для всех колонок в матрицу (vapply быстрый по колонкам)
      masks <- vapply(private$data, private$get_missing_mask_single, logical(nrow(private$data)))
      # Если только одна колонка, vapply вернёт вектор, приведём к матрице
      if (is.vector(masks)) masks <- matrix(masks, ncol = 1)

      rows_with_missing <- rowSums(masks, na.rm = TRUE) > 0
      total_rows <- nrow(private$data)
      count <- sum(rows_with_missing)
      percentage <- if (total_rows > 0) round((count / total_rows) * 100, 2) else 0

      list(rows = total_rows, count = count, percentage = percentage)
    },

    ### ---------- Пропуски ----------
    clear_missing = function(method = "mean") {
      # method: "delete" или "mean"
      if (is.null(private$data) || nrow(private$data) == 0) return(invisible(NULL))

      if (method == "delete") {
        masks <- vapply(private$data, private$get_missing_mask_single, logical(nrow(private$data)))
        if (is.vector(masks)) masks <- matrix(masks, ncol = 1)
        rows_with_missing <- rowSums(masks, na.rm = TRUE) > 0
        private$data <- private$data[!rows_with_missing, , drop = FALSE]
        # обновляем мета
        private$detect_columns()
        private$calculate_stats()
        return(invisible(NULL))
      } else if (method == "mean") {
        # numeric -> mean, factor -> moda
        # используем уже посчитанные stats (если их нет, пересчитать)
        private$calculate_stats(c(private$numeric_cols, private$factor_cols))

        # числовые колонны
        for (cn in private$numeric_cols) {
          vec <- private$data[[cn]]
          mask <- private$get_missing_mask_single(vec)
          if (any(mask, na.rm = TRUE)) {
            mean_val <- private$stats$mean[[cn]]
            # если mean_val NA (все значения NA), пропускаем
            if (!is.na(mean_val)) private$data[[cn]][mask] <- mean_val
          }
        }

        # факторные колонны
        for (cn in private$factor_cols) {
          vec <- private$data[[cn]]
          mask <- private$get_missing_mask_single(vec)
          if (any(mask, na.rm = TRUE)) {
            most_freq <- private$stats$moda[[cn]]
            if (!is.na(most_freq)) {
              # Убедимся, что уровень присутствует для фактора
              if (is.factor(private$data[[cn]])) {
                if (!(most_freq %in% levels(private$data[[cn]]))) {
                  levels(private$data[[cn]]) <- c(levels(private$data[[cn]]), most_freq)
                }
                private$data[[cn]][mask] <- most_freq
              } else {
                private$data[[cn]][mask] <- most_freq
              }
            }
          }
        }

        # после замены пересчитаем статистику только для затронутых столбцов
        cols_touched <- c(private$numeric_cols, private$factor_cols)
        private$detect_columns()
        private$calculate_stats(cols_touched)
        return(invisible(NULL))
      } else {
        stop("clear_missing: неизвестный method (use 'delete' or 'mean')")
      }
    },

    ### ---------- Выбросы ----------
    get_outliers_statistic = function(iqr_multiplier = 1.5) {
      if (length(private$numeric_cols) == 0) {
        return(list(total_outliers = 0, numeric_columns = character(0), outliers_by_column = list()))
      }

      # убедимся, что Q1 и Q3 рассчитаны
      private$calculate_stats(private$numeric_cols)

      outliers_by_column <- list()
      total_outliers <- 0

      for (cn in private$numeric_cols) {
        vec <- private$data[[cn]]
        q1 <- private$stats$Q1[[cn]]
        q3 <- private$stats$Q3[[cn]]
        if (is.null(q1) || is.null(q3) || is.na(q1) || is.na(q3)) next
        iqr <- q3 - q1
        lower <- q1 - iqr_multiplier * iqr
        upper <- q3 + iqr_multiplier * iqr

        # mask: TRUE где выброс (NA не считаем выбросом)
        mask <- (!is.na(vec)) & (vec < lower | vec > upper)
        cnt <- sum(mask, na.rm = TRUE)
        if (cnt > 0) {
          pct <- round((cnt / length(vec)) * 100, 2)
          outliers_by_column[[cn]] <- list(count = cnt, percentage = pct, lower = lower, upper = upper)
          total_outliers <- total_outliers + cnt
        }
      }

      list(total_outliers = total_outliers, numeric_columns = private$numeric_cols, outliers_by_column = outliers_by_column)
    },

    clear_outliers = function(method = "replace", iqr_multiplier = 1.5) {
      if (length(private$numeric_cols) == 0) return(invisible(NULL))

      private$calculate_stats(private$numeric_cols)

      if (method == "delete") {
        # создаём объединённую маску строк с хотя бы одним выбросом
        n <- nrow(private$data)
        if (n == 0) return(invisible(NULL))
        rows_to_remove <- rep(FALSE, n)

        for (cn in private$numeric_cols) {
          vec <- private$data[[cn]]
          q1 <- private$stats$Q1[[cn]]; q3 <- private$stats$Q3[[cn]]
          if (is.null(q1) || is.null(q3) || is.na(q1) || is.na(q3)) next
          iqr <- q3 - q1
          lower <- q1 - iqr_multiplier * iqr
          upper <- q3 + iqr_multiplier * iqr
          mask <- (!is.na(vec)) & (vec < lower | vec > upper)
          rows_to_remove <- rows_to_remove | mask
        }

        if (any(rows_to_remove)) {
          private$data <- private$data[!rows_to_remove, , drop = FALSE]
          private$detect_columns()
          private$calculate_stats()
        }

      } else if (method == "replace") {
        # заменяем выбросы на граничные значения (clamping)
        for (cn in private$numeric_cols) {
          vec <- private$data[[cn]]
          q1 <- private$stats$Q1[[cn]]; q3 <- private$stats$Q3[[cn]]
          if (is.null(q1) || is.null(q3) || is.na(q1) || is.na(q3)) next
          iqr <- q3 - q1
          lower <- q1 - iqr_multiplier * iqr
          upper <- q3 + iqr_multiplier * iqr

          # pmax/pmin векторные и сохраняют NA
          # заменим значения ниже lower на lower, выше upper на upper
          newvec <- pmin(pmax(vec, lower), upper)
          private$data[[cn]] <- newvec
        }
        private$calculate_stats(private$numeric_cols)
      } else {
        stop("clear_outliers: неизвестный method (use 'delete' or 'replace')")
      }

      invisible(NULL)
    },

    ### ---------- Масштабирование ----------
    normalize = function() {
      if (length(private$numeric_cols) == 0 || private$scaling_type != "none") return(invisible(NULL))

      # сохраняем оригинальные min/max
      private$calculate_stats(private$numeric_cols)
      private$original_params$min <- private$stats$min[private$numeric_cols]
      private$original_params$max <- private$stats$max[private$numeric_cols]

      for (cn in private$numeric_cols) {
        vec <- private$data[[cn]]
        minv <- private$original_params$min[[cn]]
        maxv <- private$original_params$max[[cn]]
        if (is.na(minv) || is.na(maxv) || maxv == minv) {
          # если нет разброса — заполняем нулями (или оставляем как NA — выбрано 0)
          private$data[[cn]] <- ifelse(is.na(vec), NA_real_, 0)
        } else {
          private$data[[cn]] <- (vec - minv) / (maxv - minv)
        }
      }

      private$scaling_type <- "normalized"
      private$calculate_stats(private$numeric_cols)
      invisible(NULL)
    },

    standardize = function() {
      if (length(private$numeric_cols) == 0 || private$scaling_type != "none") return(invisible(NULL))

      # сохраняем оригинальные mean/sd
      private$calculate_stats(private$numeric_cols)
      means <- setNames(vapply(private$numeric_cols, function(cn) {
        vec <- private$data[[cn]]; if (private$safe_all_na(vec)) NA_real_ else mean(vec, na.rm = TRUE)
      }, numeric(1)), private$numeric_cols)

      sds <- setNames(vapply(private$numeric_cols, function(cn) {
        vec <- private$data[[cn]]; if (private$safe_all_na(vec)) NA_real_ else sd(vec, na.rm = TRUE)
      }, numeric(1)), private$numeric_cols)

      private$original_params$mean <- as.list(means)
      private$original_params$sd   <- as.list(sds)

      for (cn in private$numeric_cols) {
        vec <- private$data[[cn]]
        meanv <- private$original_params$mean[[cn]]
        sdv   <- private$original_params$sd[[cn]]
        if (is.na(meanv) || is.na(sdv) || sdv == 0) {
          private$data[[cn]] <- ifelse(is.na(vec), NA_real_, 0)
        } else {
          private$data[[cn]] <- (vec - meanv) / sdv
        }
      }

      private$scaling_type <- "standardized"
      private$calculate_stats(private$numeric_cols)
      invisible(NULL)
    },

    denormalize = function() {
      if (length(private$numeric_cols) == 0 || private$scaling_type == "none") return(invisible(NULL))

      if (private$scaling_type == "normalized") {
        mins <- private$original_params$min
        maxs <- private$original_params$max
        for (cn in private$numeric_cols) {
          vec <- private$data[[cn]]
          orig_min <- mins[[cn]]; orig_max <- maxs[[cn]]
          if (!is.null(orig_min) && !is.null(orig_max) && !is.na(orig_min) && !is.na(orig_max) && orig_max > orig_min) {
            private$data[[cn]] <- vec * (orig_max - orig_min) + orig_min
          }
        }
      } else if (private$scaling_type == "standardized") {
        means <- private$original_params$mean
        sds   <- private$original_params$sd
        for (cn in private$numeric_cols) {
          vec <- private$data[[cn]]
          orig_mean <- means[[cn]]; orig_sd <- sds[[cn]]
          if (!is.null(orig_mean) && !is.null(orig_sd) && !is.na(orig_mean) && !is.na(orig_sd) && orig_sd > 0) {
            private$data[[cn]] <- vec * orig_sd + orig_mean
          }
        }
      }

      private$scaling_type <- "none"
      private$calculate_stats(private$numeric_cols)
      invisible(NULL)
    },

    ### ---------- Переименование / удаление столбцов ----------
    rename_columns = function(name_mapping) {
      # name_mapping - именованный вектор: names(name_mapping) = старые имена, 
      # значения = новые имена, т.е. c("old_name" = "new_name", ...)
      if (is.null(name_mapping) || length(name_mapping) == 0) return(invisible(NULL))

      for (old_name in names(name_mapping)) {
        new_name <- name_mapping[[old_name]]
        if (old_name %in% names(private$data) && new_name != old_name && nzchar(new_name)) {
          names(private$data)[names(private$data) == old_name] <- new_name

          # обновляем списки колонок
          private$numeric_cols[private$numeric_cols == old_name] <- new_name
          private$factor_cols[private$factor_cols == old_name]   <- new_name
          private$no_type_cols[private$no_type_cols == old_name] <- new_name

          # обновляем stats: переносим значения
          for (field in names(private$stats)) {
            private$stats[[field]] <- private$rename_in_named_list(private$stats[[field]], old_name, new_name)
          }

          # обновляем original_params (если есть)
          if (!is.null(private$original_params$min)) {
            private$original_params$min <- private$rename_in_named_list(private$original_params$min, old_name, new_name)
          }
          if (!is.null(private$original_params$max)) {
            private$original_params$max <- private$rename_in_named_list(private$original_params$max, old_name, new_name)
          }
          if (!is.null(private$original_params$mean)) {
            private$original_params$mean <- private$rename_in_named_list(private$original_params$mean, old_name, new_name)
          }
          if (!is.null(private$original_params$sd)) {
            private$original_params$sd <- private$rename_in_named_list(private$original_params$sd, old_name, new_name)
          }
        }
      }
      invisible(NULL)
    },

    remove_columns = function(columns_to_remove) {
      if (is.null(columns_to_remove) || length(columns_to_remove) == 0) return(invisible(NULL))

      cols_present <- intersect(columns_to_remove, names(private$data))
      if (length(cols_present) == 0) return(invisible(NULL))

      private$data <- private$data[, !names(private$data) %in% cols_present, drop = FALSE]

      # удаляем из списков типов
      private$numeric_cols <- setdiff(private$numeric_cols, cols_present)
      private$factor_cols  <- setdiff(private$factor_cols, cols_present)
      private$no_type_cols <- setdiff(private$no_type_cols, cols_present)

      # очищаем stats
      for (field in names(private$stats)) {
        for (col in cols_present) private$stats[[field]][[col]] <- NULL
      }

      # очищаем original_params
      if (!is.null(private$original_params$min)) for (col in cols_present) private$original_params$min[[col]] <- NULL
      if (!is.null(private$original_params$max)) for (col in cols_present) private$original_params$max[[col]] <- NULL
      if (!is.null(private$original_params$mean)) for (col in cols_present) private$original_params$mean[[col]] <- NULL
      if (!is.null(private$original_params$sd))   for (col in cols_present) private$original_params$sd[[col]] <- NULL

      private$detect_columns()
      invisible(NULL)
    },

    ### ---------- Изменение типов признаков ----------
    set_factor_columns = function(columns) {
      if (is.null(columns) || length(columns) == 0) return(invisible(NULL))

      cols_present <- intersect(columns, names(private$data))
      if (length(cols_present) == 0) return(invisible(NULL))

      for (col in cols_present) {
        # Преобразуем в фактор
        private$data[[col]] <- as.factor(private$data[[col]])
      }

      # Обновляем типы колонок и пересчитываем статистику
      private$detect_columns()
      private$calculate_stats(cols_present)
      invisible(NULL)
    },

    set_numeric_columns = function(columns) {
      if (is.null(columns) || length(columns) == 0) return(invisible(NULL))
      
      cols_present <- intersect(columns, names(private$data))
      if (length(cols_present) == 0) return(invisible(NULL))

      for (col in cols_present) {
        # Преобразуем в числовой тип
        vec <- private$data[[col]]
        # Пытаемся преобразовать в числовой, оставляя NA для нечисловых значений
        numeric_vec <- as.numeric(as.character(vec))
        
        # Проверяем, удалось ли преобразование (не все NA)
        if (!all(is.na(numeric_vec))) {
          private$data[[col]] <- numeric_vec
        }
      }
      
      # Обновляем типы колонок и пересчитываем статистику
      private$detect_columns()
      private$calculate_stats(cols_present)
      invisible(NULL)
    }
  )
)
