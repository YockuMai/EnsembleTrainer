library(R6)
library(caret)  # для предобработки
library(dplyr)  # для манипуляций с данными

DataPreprocess <- R6::R6Class("DataPreprocess",
  private = list(
    original_data = NULL,
    processed_data = NULL,
    scalers = list(),
    preprocessing_steps = list()
  ),

  public = list(
    initialize = function() {
      private$original_data <- NULL
      private$processed_data <- NULL
      private$scalers <- list()
      private$preprocessing_steps <- list()
    },

    set_data = function(data) {
      if (!is.data.frame(data)) {
        stop("Данные должны быть data.frame")
      }
      private$original_data <- data
      private$processed_data <- data
      private$preprocessing_steps <- list()
      message("Данные установлены")
    },

    handle_missing = function(method = "mean") {
      if (is.null(private$processed_data)) {
        stop("Сначала загрузите данные")
      }

      data <- private$processed_data

      # Определяем числовые и категориальные столбцы
      num_cols <- sapply(data, is.numeric)
      cat_cols <- sapply(data, is.factor) | sapply(data, is.character)

      if (method == "remove") {
        data <- na.omit(data)
        step <- "Удалены строки с пропущенными значениями"
      } else if (method %in% c("mean", "median")) {
        for (col in names(data)[num_cols]) {
          if (any(is.na(data[[col]]))) {
            if (method == "mean") {
              data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
            } else {
              data[[col]][is.na(data[[col]])] <- median(data[[col]], na.rm = TRUE)
            }
          }
        }
        step <- paste("Пропущенные числовые значения заменены на", method)
      } else if (method == "mode") {
        for (col in names(data)[cat_cols]) {
          if (any(is.na(data[[col]]))) {
            mode_val <- names(sort(table(data[[col]]), decreasing = TRUE))[1]
            data[[col]][is.na(data[[col]])] <- mode_val
          }
        }
        step <- "Пропущенные категориальные значения заменены на моду"
      } else {
        stop("Неподдерживаемый метод обработки пропусков")
      }

      private$processed_data <- data
      private$preprocessing_steps <- c(private$preprocessing_steps, step)
      message(step)
    },

    encode_categorical = function() {
      if (is.null(private$processed_data)) {
        stop("Сначала загрузите данные")
      }

      data <- private$processed_data
      cat_cols <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]

      if (length(cat_cols) > 0) {
        data[cat_cols] <- lapply(data[cat_cols], function(x) {
          if (is.character(x)) as.factor(x) else x
        })
        step <- "Категориальные переменные преобразованы в факторы"
        private$processed_data <- data
        private$preprocessing_steps <- c(private$preprocessing_steps, step)
        message(step)
      } else {
        message("Категориальные переменные не найдены")
      }
    },

    handle_outliers = function(method = "iqr", threshold = 1.5, columns = NULL) {
      if (is.null(private$processed_data)) {
        stop("Сначала загрузите данные")
      }

      data <- private$processed_data

      # Определяем числовые столбцы
      num_cols <- names(data)[sapply(data, is.numeric)]

      if (length(num_cols) == 0) {
        warning("Нет числовых столбцов для обработки выбросов")
      }

      # Если указаны конкретные столбцы, фильтруем
      if (!is.null(columns)) {
        num_cols <- intersect(num_cols, columns)
        if (length(num_cols) == 0) {
          warning("Указанные столбцы не найдены или не являются числовыми")
        }
      }

      #original_rows <- nrow(data)
      outliers_removed <- 0

      for (col in num_cols) {
        Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
        Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
        IQR <- Q3 - Q1
        lower_bound <- Q1 - threshold * IQR
        upper_bound <- Q3 + threshold * IQR
      }

      if (method == "iqr") {
          # Находим индексы выбросов
          outlier_indices <- which(data[[col]] < lower_bound | data[[col]] > upper_bound)
          if (length(outlier_indices) > 0) {
            data <- data[-outlier_indices, ]
            outliers_removed <- outliers_removed + length(outlier_indices)
          }
        step <- sprintf("Удалено %d выбросов методом IQR (порог %.1f)", outliers_removed, threshold)

      } else if (method == "winsorize") {
        # Winsorization - ограничение выбросов
          data[[col]][data[[col]] < lower_bound] <- lower_bound
          data[[col]][data[[col]] > upper_bound] <- upper_bound
        step <- sprintf("Применена winsorization (порог %.1f) для ограничения выбросов", threshold)

      } else {
        stop("Неподдерживаемый метод обработки выбросов")
      }

      private$processed_data <- data
      private$preprocessing_steps <- c(private$preprocessing_steps, step)
      message(step)
    },

    scale_features = function(method = "standard") {
      if (is.null(private$processed_data)) {
        stop("Сначала загрузите данные")
      }

      data <- private$processed_data
      num_cols <- names(data)[sapply(data, is.numeric)]

      if (length(num_cols) == 0) {
        warning("Нет числовых столбцов для масштабирования")
        return()
      }

      if (method == "standard") {
        # Стандартизация (Z-score)
        for (col in num_cols) {
          mean_val <- mean(data[[col]], na.rm = TRUE)
          sd_val <- sd(data[[col]], na.rm = TRUE)
          data[[col]] <- (data[[col]] - mean_val) / sd_val
          private$scalers[[col]] <- list(mean = mean_val, sd = sd_val, method = "standard")
        }
        step <- "Применена стандартизация числовых признаков"
      } else if (method == "minmax") {
        # Min-Max scaling
        for (col in num_cols) {
          min_val <- min(data[[col]], na.rm = TRUE)
          max_val <- max(data[[col]], na.rm = TRUE)
          data[[col]] <- (data[[col]] - min_val) / (max_val - min_val)
          private$scalers[[col]] <- list(min = min_val, max = max_val, method = "minmax")
        }
        step <- "Применено min-max масштабирование (нормализация) числовых признаков"
      } else {
        stop("Неподдерживаемый метод масштабирования")
      }

      private$processed_data <- data
      private$preprocessing_steps <- c(private$preprocessing_steps, step)
      message(step)
    },

    preprocess = function(data, missing_method = "mean", outlier_method = NULL, outlier_threshold = 1.5,
                         scale_method = "standard") {
      self$set_data(data)
      self$handle_missing(missing_method)
      if (!is.null(outlier_method)) {
        self$handle_outliers(outlier_method, outlier_threshold)
      }
      self$encode_categorical()
      self$scale_features(scale_method)
      message("Предобработка завершена")
    },

    get_original_data = function() {
      return(private$original_data)
    },

    get_processed_data = function() {
      return(private$processed_data)
    },

    get_train_data = function() {
      return(private$train_data)
    },

    get_test_data = function() {
      return(private$test_data)
    },

    get_preprocessing_summary = function() {
      return(list(
        original_rows = if (!is.null(private$original_data)) nrow(private$original_data) else 0,
        processed_rows = if (!is.null(private$processed_data)) nrow(private$processed_data) else 0,
        steps = private$preprocessing_steps
      ))
    },

    reset = function() {
      private$processed_data <- private$original_data
      private$scalers <- list()
      private$preprocessing_steps <- list()
      message("Сброс к исходным данным")
    }
  )
)
