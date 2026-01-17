PreprocessData <- R6::R6Class("PreprocessData",
  private = list(
    data = NULL,
    numeric_cols = NULL,
    factor_cols = NULL,
    min = list(),
    max = list(),
    Q1 = list(),
    Q3 = list(),
    mean = list(),
    moda = list(),
    scaling_type = "none",
    original_params = list(),
    stat_fields = c("min", "max", "Q1", "Q3", "mean", "moda"),
    detect_functions = list(),

    # Вспомогательная функция для определения числовых столбцов
    detect_numeric_columns = function() {
      numeric_cols <- sapply(private$data, function(col) {
        is.numeric(col) || is.integer(col)
      })
      private$numeric_cols <- names(private$data)[numeric_cols]
    },

    # Вспомогательная функция для определения категориальных столбцов
    detect_factor_columns = function() {
      factor_cols <- sapply(private$data, function(col) {
        is.factor(col) || (is.character(col) && length(unique(col)) < sqrt(nrow(private$data)))
      })
      private$factor_cols <- names(private$data)[factor_cols]
    },

    detect_min_values = function() {
      mins <- list()

      for (col_name in private$numeric_cols) {
        col_data <- private$data[[col_name]]
        mins[[col_name]] <- min(col_data, na.rm = TRUE)
      }

      private$min <- mins
    },

    detect_max_values = function() {
      maxs <- list()

      for (col_name in private$numeric_cols) {
        col_data <- private$data[[col_name]]
        maxs[[col_name]] <- max(col_data, na.rm = TRUE)
      }

      private$max <- maxs
    },

    detect_Q1_values = function() {
      q1s <- list()

      for (col_name in private$numeric_cols) {
        col_data <- private$data[[col_name]]
        q1s[[col_name]] <- quantile(col_data, 0.25, na.rm = TRUE)
      }

      private$Q1 <- q1s
    },

    detect_Q3_values = function() {
      q3s <- list()

      for (col_name in private$numeric_cols) {
        col_data <- private$data[[col_name]]
        q3s[[col_name]] <- quantile(col_data, 0.75, na.rm = TRUE)
      }

      private$Q3 <- q3s
    },

    detect_mean_values = function() {
      means <- list()

      for (col_name in private$numeric_cols) {
        col_data <- private$data[[col_name]]
        means[[col_name]] <- mean(col_data, na.rm = TRUE)
      }

      private$mean <- means
    },

    detect_moda_values = function() {
      modas <- list()

      for (col_name in private$factor_cols) {
        col_data <- private$data[[col_name]]
    
        # Удаляем NA для корректного подсчета
        valid_data <- na.omit(col_data)
    
        if (length(valid_data) > 0) {
          # Создаем таблицу частот
          freq_table <- table(valid_data)
      
          # Находим значение с максимальной частотой
          # Если несколько значений имеют одинаковую максимальную частоту,
          # выбираем первое
          moda <- names(freq_table)[which.max(freq_table)]
          modas[[col_name]] <- moda
        } else {
          # Если все значения NA, устанавливаем NA
          modas[[col_name]] <- NA
        }
      }

      private$moda <- modas
    },

    calculate_statistic = function() {
      for (func_name in private$detect_functions) {
        private[[func_name]]()
      }
    },

    get_missing_mask = function(col_data) {
      if (is.numeric(col_data) || is.integer(col_data)) {
        return(is.na(col_data))
      } else {
        col_char <- as.character(col_data)
        return(is.na(col_char) | 
               col_char == "" | 
               col_char == "N/A" | 
               col_char == "NULL" | 
               col_char == "NA" | 
               col_char == "null" | 
               col_char == "n/a")
        }
    },

    find_missing_rows = function() {
      rows_with_missing <- integer(nrow(private$data))

      for (col_name in names(private$data)) {
        col_data <- private$data[[col_name]]
        missing_mask <- private$get_missing_mask(col_data)
        rows_with_missing[missing_mask] <- 1
      }

      return(rows_with_missing)
    },

    get_outlier_bounds = function(col_name, iqr_multiplier = 1.5) {
      q1 <- private$Q1[[col_name]]
      q3 <- private$Q3[[col_name]]
      iqr <- q3 - q1

      lower_bound <- q1 - iqr_multiplier * iqr
      upper_bound <- q3 + iqr_multiplier * iqr

      return(list(lower = lower_bound, upper = upper_bound))
    }
  ),

  public = list(
    initialize = function(data) {
      private$detect_functions <- paste0("detect_", private$stat_fields, "_values")
      private$data <- data
      private$detect_numeric_columns()
      private$detect_factor_columns()
      private$calculate_statistic()
    },

    get_data = function() {
      return(private$data)
    },

    get_statistic = function() {
      result <- list()
      for (field in private$stat_fields) {
        result[[field]] <- private[[field]]
      }
      return(result)
    },

    get_missing_statistic = function() {
      if (is.null(private$data) || nrow(private$data) == 0) {
        return(list(count = 0, percentage = 0))
      }

      total_rows <- nrow(private$data)
      rows_with_missing <- private$find_missing_rows()

      count <- sum(rows_with_missing)
      percentage <- (count / total_rows) * 100

      return(list(
        rows = total_rows,
        count = count,
        percentage = round(percentage, 2)
      ))
    },

    clear_missing = function(method = "mean") {
      if (method == "delete") {
        # Удаление строк с пропущенными значениями
        rows_with_missing <- private$find_missing_rows()

        # Оставляем только строки без пропусков
        private$data <- private$data[rows_with_missing == 0, , drop = FALSE]

      } else if (method == "mean") {
        # Замена пропущенных значений

        # Обработка числовых столбцов - замена на среднее
        for (col_name in private$numeric_cols) {
          col_data <- private$data[[col_name]]
          missing_mask <- private$get_missing_mask(col_data)

          if (any(missing_mask)) {
            mean_val <- private$mean[[col_name]]
            private$data[[col_name]][missing_mask] <- mean_val
          }
        }

        # Обработка категориальных столбцов - замена на моду
        for (col_name in private$factor_cols) {
          col_data <- private$data[[col_name]]
          missing_mask <- private$get_missing_mask(col_data)

          if (any(missing_mask)) {
            most_frequent <- private$moda[[col_name]]
            private$data[[col_name]][missing_mask] <- most_frequent
          }
        }
      }

      # После обработки обновляем статистику
      private$calculate_statistic()
    },

    get_outliers_statistic = function(iqr_multiplier = 1.5) {
      if (length(private$numeric_cols) == 0) {
        return(list(
          total_outliers = 0,
          numeric_columns = character(0),
          outliers_by_column = list()
        ))
      }

      outliers_by_column <- list()
      total_outliers <- 0

      for (col_name in private$numeric_cols) {
        col_data <- private$data[[col_name]]
        bounds <- private$get_outlier_bounds(col_name, iqr_multiplier)

        # Находим выбросы
        outliers_mask <- col_data < bounds$lower | col_data > bounds$upper
        count_outliers <- sum(outliers_mask, na.rm = TRUE)

        if (count_outliers > 0) {
          percentage <- round((count_outliers / length(col_data)) * 100, 2)
          outliers_by_column[[col_name]] <- list(
            count = count_outliers,
            percentage = percentage
          )
          total_outliers <- total_outliers + count_outliers
        }
      }

      return(list(
        total_outliers = total_outliers,
        numeric_columns = private$numeric_cols,
        outliers_by_column = outliers_by_column
      ))
    },

    clear_outliers = function(method = "replace", iqr_multiplier = 1.5) {
      if (length(private$numeric_cols) == 0) {
        return()
      }

      if (method == "delete") {
        # Удаление строк с выбросами
        rows_to_keep <- rep(TRUE, nrow(private$data))

        for (col_name in private$numeric_cols) {
          col_data <- private$data[[col_name]]
          bounds <- private$get_outlier_bounds(col_name, iqr_multiplier)

          # Отмечаем строки с выбросами для удаления
          outlier_mask <- col_data < bounds$lower | col_data > bounds$upper
          rows_to_keep[outlier_mask] <- FALSE
        }

        # Удаляем строки с выбросами
        private$data <- private$data[rows_to_keep, , drop = FALSE]

      } else if (method == "replace") {
        # Замена выбросов на граничные значения

        for (col_name in private$numeric_cols) {
          col_data <- private$data[[col_name]]
          bounds <- private$get_outlier_bounds(col_name, iqr_multiplier)

          # Заменяем выбросы
          private$data[[col_name]] <- ifelse(
            col_data < bounds$lower, 
            bounds$lower, 
            ifelse(col_data > bounds$upper, bounds$upper, col_data)
          )
        }
      }

      # После обработки обновляем статистику
      private$calculate_statistic()
    },

    normalize = function() {
      if (length(private$numeric_cols) == 0 || private$scaling_type != "none") {
        return()
      }

      private$original_params$min <- private$min
      private$original_params$max <- private$max

      for (col_name in private$numeric_cols) {
        col_data <- private$data[[col_name]]
        min_val <- private$min[[col_name]]
        max_val <- private$max[[col_name]]

        if (max_val > min_val) {
          private$data[[col_name]] <- (col_data - min_val) / (max_val - min_val)
        }
      }

      private$scaling_type <- "normalized"
      private$calculate_statistic()
    },

    standardize = function() {
      if (length(private$numeric_cols) == 0 || private$scaling_type != "none") {
        return()
      }

      private$original_params$mean <- private$mean
      private$original_params$sd <- list()

      for (col_name in private$numeric_cols) {
        col_data <- private$data[[col_name]]
        mean_val <- private$mean[[col_name]]
        sd_val <- sd(col_data, na.rm = TRUE)

        private$original_params$sd[[col_name]] <- sd_val

        if (sd_val > 0) {
          private$data[[col_name]] <- (col_data - mean_val) / sd_val
        }
      }

      private$scaling_type <- "standardized"
      private$calculate_statistic()
    },

    denormalize = function() {
      if (length(private$numeric_cols) == 0 || private$scaling_type == "none") {
        return()
      }

      if (private$scaling_type == "normalized") {
        for (col_name in private$numeric_cols) {
          col_data <- private$data[[col_name]]
          orig_min <- private$original_params$min[[col_name]]
          orig_max <- private$original_params$max[[col_name]]

          if (!is.null(orig_min) && !is.null(orig_max) && orig_max > orig_min) {
            private$data[[col_name]] <- col_data * (orig_max - orig_min) + orig_min
          }
        }
      } else if (private$scaling_type == "standardized") {
        for (col_name in private$numeric_cols) {
          col_data <- private$data[[col_name]]
          orig_mean <- private$original_params$mean[[col_name]]
          orig_sd <- private$original_params$sd[[col_name]]

          if (!is.null(orig_mean) && !is.null(orig_sd) && orig_sd > 0) {
            private$data[[col_name]] <- col_data * orig_sd + orig_mean
          }
        }
      }

      private$scaling_type <- "none"
      private$calculate_statistic()
    },

    rename_column = function(name_mapping) {
      # name_mapping - именованный вектор: c("старое_имя" = "новое_имя", ...)
      if (is.null(name_mapping) || length(name_mapping) == 0) {
        return()
      }

      # Переименовываем столбцы в данных
      for (old_name in names(name_mapping)) {
        new_name <- name_mapping[old_name]
        if (old_name %in% names(private$data) && new_name != old_name) {
          names(private$data)[names(private$data) == old_name] <- new_name

          # Обновляем списки столбцов
          if (old_name %in% private$numeric_cols) {
            private$numeric_cols <- replace(private$numeric_cols, 
                                           private$numeric_cols == old_name, 
                                           new_name)
          }
          if (old_name %in% private$factor_cols) {
            private$factor_cols <- replace(private$factor_cols, 
                                          private$factor_cols == old_name, 
                                          new_name)
          }

          # Обновляем статистику - переименовываем ключи
          for (field in private$stat_fields) {
            if (!is.null(private[[field]][[old_name]])) {
              private[[field]][[new_name]] <- private[[field]][[old_name]]
              private[[field]][[old_name]] <- NULL
            }
          }

          # Обновляем оригинальные параметры масштабирования
          if (private$scaling_type == "normalized") {
            if (!is.null(private$original_params$min[[old_name]])) {
              private$original_params$min[[new_name]] <- private$original_params$min[[old_name]]
              private$original_params$min[[old_name]] <- NULL
            }
            if (!is.null(private$original_params$max[[old_name]])) {
              private$original_params$max[[new_name]] <- private$original_params$max[[old_name]]
              private$original_params$max[[old_name]] <- NULL
            }
          } else if (private$scaling_type == "standardized") {
            if (!is.null(private$original_params$mean[[old_name]])) {
              private$original_params$mean[[new_name]] <- private$original_params$mean[[old_name]]
              private$original_params$mean[[old_name]] <- NULL
            }
            if (!is.null(private$original_params$sd[[old_name]])) {
              private$original_params$sd[[new_name]] <- private$original_params$sd[[old_name]]
              private$original_params$sd[[old_name]] <- NULL
            }
          }
        }
      }
    },

    remove_column = function(columns_to_remove) {
      if (is.null(columns_to_remove) || length(columns_to_remove) == 0) {
        return()
      }

      # Удаляем столбцы из данных
      private$data <- private$data[, !names(private$data) %in% columns_to_remove, drop = FALSE]

      # Обновляем списки типов столбцов
      private$numeric_cols <- setdiff(private$numeric_cols, columns_to_remove)
      private$factor_cols <- setdiff(private$factor_cols, columns_to_remove)

      # Удаляем из статистических полей
      for (field in private$stat_fields) {
        for (col in columns_to_remove) {
          private[[field]][[col]] <- NULL
        }
      }

      # Удаляем из параметров масштабирования
      if (private$scaling_type == "normalized") {
        for (col in columns_to_remove) {
          private$original_params$min[[col]] <- NULL
          private$original_params$max[[col]] <- NULL
        }
      } else if (private$scaling_type == "standardized") {
        for (col in columns_to_remove) {
          private$original_params$mean[[col]] <- NULL
          private$original_params$sd[[col]] <- NULL
        }
      }
    }
  )
)
