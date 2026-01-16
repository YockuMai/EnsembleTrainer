PreprocessData <- R6::R6Class("PreprocessData",
  private = list(
    data = NULL,
    numeric_cols = NULL,
    factor_cols = NULL,

    # Вспомогательная функция для определения числовых столбцов
    detect_numeric_columns = function(data) {
      numeric_cols <- sapply(data, function(col) {
        is.numeric(col) || is.integer(col)
      })
      return(which(numeric_cols)) # nolint: return_linter.
    },

    # Вспомогательная функция для определения категориальных столбцов
    detect_factor_columns = function(data) {
      factor_cols <- sapply(data, function(col) {
        is.factor(col) || (is.character(col) && length(unique(col)) < sqrt(nrow(data)))
      })
      return(which(factor_cols))
    },

    # Обработка выбросов с использованием метода IQR
    handle_outliers = function(data, iqr_multiplier = 1.5, method = "replace") {
      numeric_cols <- private$detect_numeric_columns(data)

      if (method == "remove") {
        # Удаление строк с выбросами
        keep_rows <- rep(TRUE, nrow(data))
        for (col_name in names(data)[numeric_cols]) {
          if (length(unique(data[[col_name]])) > 5) { # Только для непрерывных переменных
            q <- quantile(data[[col_name]], probs = c(0.25, 0.75), na.rm = TRUE)
            iqr <- q[2] - q[1]
            lower_bound <- q[1] - iqr_multiplier * iqr
            upper_bound <- q[2] + iqr_multiplier * iqr

            keep_rows <- keep_rows & (data[[col_name]] >= lower_bound & data[[col_name]] <= upper_bound)
          }
        }
        data <- data[keep_rows, , drop = FALSE]
      } else { # method == "replace"
        # Замена выбросов границами
        for (col_name in names(data)[numeric_cols]) {
          if (length(unique(data[[col_name]])) > 5) { # Только для непрерывных переменных
            q <- quantile(data[[col_name]], probs = c(0.25, 0.75), na.rm = TRUE)
            iqr <- q[2] - q[1]
            lower_bound <- q[1] - iqr_multiplier * iqr
            upper_bound <- q[2] + iqr_multiplier * iqr

            data[[col_name]] <- ifelse(data[[col_name]] < lower_bound, lower_bound,
                                       ifelse(data[[col_name]] > upper_bound, upper_bound,
                                             data[[col_name]]))
          }
        }
      }
      return(data)
    },

    # Нормализация числовых данных (min-max scaling)
    normalize_data = function(data) {
      numeric_cols <- private$detect_numeric_columns(data)

      for (col_name in names(data)[numeric_cols]) {
        if (var(data[[col_name]], na.rm = TRUE) > 0.001) { # Только если есть вариация
          min_val <- min(data[[col_name]], na.rm = TRUE)
          max_val <- max(data[[col_name]], na.rm = TRUE)

          if (min_val != max_val) {
            data[[col_name]] <- (data[[col_name]] - min_val) / (max_val - min_val)
          }
        }
      }
      return(data)
    },

    # Стандартизация числовых данных (z-score)
    standardize_data = function(data) {
      numeric_cols <- private$detect_numeric_columns(data)

      for (col_name in names(data)[numeric_cols]) {
        if (var(data[[col_name]], na.rm = TRUE) > 0.001) { # Только если есть вариация
          mean_val <- mean(data[[col_name]], na.rm = TRUE)
          sd_val <- sd(data[[col_name]], na.rm = TRUE)

          if (sd_val > 0) {
            data[[col_name]] <- (data[[col_name]] - mean_val) / sd_val
          }
        }
      }
      return(data)
    },

    # Обработка пропущенных значений
    handle_missing = function(data, method = "mean") {
      if (method == "delete") {
        # Удаление строк с пропущенными значениями (учитываем все типы пропущенных значений)
        rows_to_keep <- rep(TRUE, nrow(data))
        
        for (col_name in names(data)) {
          col_data <- data[[col_name]]
          
          if (is.numeric(col_data) || is.integer(col_data)) {
            # Для числовых данных - проверяем NA
            missing_mask <- is.na(col_data)
          } else {
            # Для категориальных данных - проверяем различные представления пропущенных значений
            col_char <- as.character(col_data)
            missing_mask <- is.na(col_char) | 
                           col_char == "" | 
                           col_char == "N/A" | 
                           col_char == "NULL" | 
                           col_char == "NA" | 
                           col_char == "null" | 
                           col_char == "n/a"
          }
          
          # Отмечаем строки с пропущенными значениями для удаления
          rows_to_keep[missing_mask] <- FALSE
        }
        
        data <- data[rows_to_keep, , drop = FALSE]
      } else if (method == "mean") {
        # Замена пропущенных значений: среднее для числовых, мода для категориальных
        numeric_cols <- private$detect_numeric_columns(data)
        factor_cols <- private$detect_factor_columns(data)

        # Обработка числовых столбцов - замена на среднее
        for (col_name in names(data)[numeric_cols]) {
          if (any(is.na(data[[col_name]]))) {
            mean_val <- mean(data[[col_name]], na.rm = TRUE)
            data[[col_name]] <- ifelse(is.na(data[[col_name]]), mean_val, data[[col_name]])
          }
        }

        # Обработка категориальных столбцов - замена на наиболее частое значение
        for (col_name in names(data)[factor_cols]) {
          if (any(is.na(data[[col_name]]) | data[[col_name]] == "" | 
                 data[[col_name]] == "N/A" | data[[col_name]] == "NULL" | 
                 data[[col_name]] == "NA" | data[[col_name]] == "null" | 
                 data[[col_name]] == "n/a")) {
            
            # Преобразуем в character для корректной обработки
            col_data <- as.character(data[[col_name]])
            
            # Удаляем все типы пропущенных значений для подсчета моды
            valid_values <- col_data[!(is.na(col_data) | 
                                      col_data == "" | 
                                      col_data == "N/A" | 
                                      col_data == "NULL" | 
                                      col_data == "NA" | 
                                      col_data == "null" | 
                                      col_data == "n/a")]
            
            if (length(valid_values) > 0) {
              # Находим моду (наиболее частое значение)
              freq_table <- table(valid_values)
              most_frequent <- names(freq_table)[which.max(freq_table)]
              
              # Заменяем все типы пропущенных значений на моду
              data[[col_name]] <- ifelse(is.na(data[[col_name]]) | 
                                        data[[col_name]] == "" | 
                                        data[[col_name]] == "N/A" | 
                                        data[[col_name]] == "NULL" | 
                                        data[[col_name]] == "NA" | 
                                        data[[col_name]] == "null" | 
                                        data[[col_name]] == "n/a", 
                                        most_frequent, 
                                        data[[col_name]])
            }
          }
        }
      }

      return(data)
    },

    # Удаление столбцов
    remove_columns = function(data, columns_to_remove) {
      if (!is.null(columns_to_remove) && length(columns_to_remove) > 0) {
        return(data[, !(names(data) %in% columns_to_remove), drop = FALSE])
      }
      return(data)
    }
  ),

  public = list(
    initialize = function() {
      private$data <- NULL
      private$numeric_cols <- NULL
      private$factor_cols <- NULL
    },

    # Основной метод предобработки
    preprocess = function(data,
                         handle_outliers = FALSE,
                         outlier_method = "replace",
                         iqr_multiplier = 1.5,
                         normalize = FALSE,
                         standardize = FALSE,
                         handle_missing = FALSE,
                         missing_method = "mean",
                         remove_columns = NULL) {

      if (is.null(data) || nrow(data) == 0) {
        stop("Input data is empty or NULL")
      }

      # Удаление столбцов
      if (!is.null(remove_columns)) {
        data <- private$remove_columns(data, remove_columns)
      }

      # Обработка пропущенных значений
      if (handle_missing) {
        data <- private$handle_missing(data, missing_method)
      }

      # Обработка выбросов
      if (handle_outliers) {
        data <- private$handle_outliers(data, iqr_multiplier, outlier_method)
      }

      # Нормализация
      if (normalize) {
        data <- private$normalize_data(data)
      }

      # Стандартизация
      if (standardize) {
        data <- private$standardize_data(data)
      }

      # Сохраняем информацию о столбцах
      private$data <- data
      private$numeric_cols <- private$detect_numeric_columns(data)
      private$factor_cols <- private$detect_factor_columns(data)

      return(data)
    },

    # Метод для получения предобработанных данных
    get_processed_data = function() {
      return(private$data)
    },

    get_numeric_cols = function() {
      return(private$numeric_cols)
    },

    get_factor_cols = function() {
      return(private$factor_cols)
    },

    # Подсчет количества строк с пропущенными значениями
    get_count_missing_rows = function(data) {
      if (is.null(data) || nrow(data) == 0) {
        return(0)
      }

      # Более эффективный подход: работаем с каждым столбцом отдельно
      total_rows <- nrow(data)
      rows_with_missing <- integer(total_rows)
      
      for (col_name in names(data)) {
        col_data <- data[[col_name]]
        
        if (is.numeric(col_data) || is.integer(col_data)) {
          # Для числовых данных - просто проверяем NA
          missing_mask <- is.na(col_data)
        } else {
          # Для категориальных данных - проверяем различные представления пропущенных значений
          col_char <- as.character(col_data)
          missing_mask <- is.na(col_char) | 
                         col_char == "" | 
                         col_char == "N/A" | 
                         col_char == "NULL" | 
                         col_char == "NA" | 
                         col_char == "null" | 
                         col_char == "n/a"
        }
        
        # Отмечаем строки, где есть пропущенные значения
        rows_with_missing[missing_mask] <- 1
      }
      
      return(sum(rows_with_missing))
    }
  )
)
