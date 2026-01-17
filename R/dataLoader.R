DataLoader <- R6::R6Class("DataLoader",
  private = list(
    data = NULL,
    error_message = NULL,
    sep = NULL,

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
        separators <- c(",", ";", "\t", "|")
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
          test_data <- read.csv(filepath, sep = best_sep, nrows = 5, stringsAsFactors = TRUE)
          !is.null(test_data) && ncol(test_data) > 1
        }, error = function() {
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
                test_data <- read.csv(filepath, sep = alt_sep, nrows = 5, stringsAsFactors = TRUE)
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

      }, error = function() {
        return(";")
      })
    }
  ),

  public = list(
    initialize = function() {
      private$data <- NULL
      private$error_message <- NULL
      private$sep <- NULL
    },

    csv_load = function(filepath, sep = NULL) {
      if (!file.exists(filepath)) {
        private$error_message <- "Файл не найден"
        return(FALSE)
      }
      if (!grepl("\\.csv$", filepath, ignore.case = TRUE)) {
        private$error_message <- "Файл должен иметь расширение .csv"
        return(FALSE)
      }

      # Если разделитель не указан, определяем автоматически
      if (is.null(sep)) {
        sep <- private$guessSeparator(filepath)
        private$sep <- sep
      }
      else {
        private$sep <- sep
      }

      private$data <- read.csv(filepath, stringsAsFactors = TRUE, sep=private$sep)
      if (nrow(private$data) == 0) {
        private$error_message <- "Файл пустой или не содержит данных"
        return(FALSE)
      }
      private$error_message <- NULL
      return(TRUE)
    },
    get_data = function() {
      return(private$data)
    },
    get_error = function() {
      return(private$error_message)
    },
    get_sep = function() {
      return(private$sep)
    }
  )
)
