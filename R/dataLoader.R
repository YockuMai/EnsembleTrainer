DataLoader <- R6::R6Class("DataLoader",
  private = list(
    data = NULL,
    error_message = NULL
  ),

  public = list(
    initialize = function() {
      # Initialization code if needed
      private$data <- NULL
      private$error_message <- NULL
    },
    # Автоматическое определение разделителя
    guessSeparator = function(filepath) {
      tryCatch({
        if (!file.exists(filepath)) {
          return(";")  # Возвращаем разделитель по умолчанию
        }

        # Читаем первые 10 строк как текст
        lines <- readLines(filepath, n = 10, warn = FALSE)

        if (length(lines) == 0) {
          return(";")
        }

        # Потенциальные разделители
        separators <- c(",", ";", "\t", "|", " ")
        sep_counts <- c()

        for (sep in separators) {
          # Подсчитываем количество разделителей в каждой строке
          counts <- sapply(lines, function(line) {
            length(strsplit(line, sep, fixed = TRUE)[[1]]) - 1
          })
          # Среднее количество разделителей
          sep_counts <- c(sep_counts, mean(counts))
        }

        # Выбираем разделитель с максимальным средним количеством
        best_sep_idx <- which.max(sep_counts)

        # Если максимальное количество разделителей = 0, возвращаем по умолчанию
        if (sep_counts[best_sep_idx] == 0) {
          return(";")
        }

        # Проверяем выбранный разделитель пробным чтением
        best_sep <- separators[best_sep_idx]
        test_result <- tryCatch({
          test_data <- read.csv(filepath, sep = best_sep, nrows = 5, stringsAsFactors = FALSE)
          !is.null(test_data) && ncol(test_data) > 1
        }, error = function(e) {
          FALSE
        })

        if (test_result) {
          return(best_sep)
        } else {
          # Если пробное чтение не удалось, пробуем следующий по частоте
          sorted_indices <- order(sep_counts, decreasing = TRUE)
          for (i in 2:length(sorted_indices)) {
            alt_sep <- separators[sorted_indices[i]]
            if (sep_counts[sorted_indices[i]] > 0) {
              test_result <- tryCatch({
                test_data <- read.csv(filepath, sep = alt_sep, nrows = 5, stringsAsFactors = FALSE)
                !is.null(test_data) && ncol(test_data) > 1
              }, error = function(e) {
                FALSE
              })
              if (test_result) {
                return(alt_sep)
              }
            }
          }
        }

        return(";")  # Возвращаем разделитель по умолчанию

      }, error = function(e) {
        return(";")
      })
    },

    csvLoad = function(filepath, sep = NULL) {
      tryCatch({
        if (!file.exists(filepath)) {
          private$error_message <- "Файл не найден"
          return(NULL)
        }
        if (!grepl("\\.csv$", filepath, ignore.case = TRUE)) {
          private$error_message <- "Файл должен иметь расширение .csv"
          return(NULL)
        }

        # Если разделитель не указан, определяем автоматически
        if (is.null(sep)) {
          sep <- self$guessSeparator(filepath)
        }

        data <- read.csv(filepath, stringsAsFactors = FALSE, sep=sep)
        if (nrow(data) == 0) {
          private$error_message <- "Файл пустой или не содержит данных"
          return(NULL)
        }
        private$data <- data
        private$error_message <- NULL
        return(TRUE)
      }, error = function(e) {
        private$error_message <- paste("Ошибка при загрузке файла:", e$message)
        return(NULL)
      })
    },
    getData = function() {
      return(private$data)
    },
    getError = function() {
      return(private$error_message)
    },
    hasData = function() {
      return(!is.null(private$data))
    }
  )
)
