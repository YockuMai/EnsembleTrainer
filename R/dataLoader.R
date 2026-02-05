DataLoader <- R6::R6Class("DataLoader",
  private = list(
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
    initialize = function() {},

    csv_load = function(filepath, sep = NULL, stringsAsFactors = FALSE) {
      if (!file.exists(filepath)) {
        stop("Файл не найден")
      }
      if (!grepl("\\.csv$", filepath, ignore.case = TRUE)) {
        stop("Файл должен иметь расширение .csv")
      }

      # Если разделитель не указан, определяем автоматически
      if (is.null(sep))
        sep <- private$guessSeparator(filepath)

      data <- read.csv(filepath, sep = sep, stringsAsFactors = stringsAsFactors)
      if (nrow(data) == 0) {
        stop("Файл пустой или не содержит данных")
      }
      return(list(
        success = TRUE,
        data = data,
        sep = sep
      ))
    }
  )
)
