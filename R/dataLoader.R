DataLoader <- R6::R6Class("DataLoader",
  private = list(
     # Автоматическое определение разделителя
    guessSeparator = function(filepath) {
      if (!file.exists(filepath)) {
        stop("Файл не найден: ", filepath)
      }
      
      lines <- tryCatch(
        readLines(filepath, n = 10, warn = FALSE),
        error = function(e) stop("Не удалось прочитать файл: ", e$message)
      )
      
      if (length(lines) == 0) {
        stop("Файл пуст или не содержит строк для анализа.")
      }
      
      # Список кандидатов-разделителей
      seps <- c(",", ";", "\t", "|")
      counts <- sapply(seps, function(s) {
        sum(stringr::str_count(lines[1:min(5, length(lines))], stringr::fixed(s)))
      })
      
      if (max(counts) == 0) {
        stop("Не удалось определить разделитель. Пожалуйста, укажите его вручную.")
      }
      
      best_sep <- seps[which.max(counts)]
      return(best_sep)
    }
  ),

  public = list(
    initialize = function() {},

    csv_load = function(filepath, sep = NULL, stringsAsFactors = FALSE) {
      # Проверки
      if (!file.exists(filepath)) {
        stop("Файл не найден. Проверьте путь и повторите попытку.")
      }
      if (!grepl("\\.csv$", filepath, ignore.case = TRUE)) {
        stop("Поддерживаются только файлы с расширением .csv")
      }
      if (file.info(filepath)$size == 0) {
        stop("Файл пуст. Загрузите корректный CSV.")
      }
      
      # Если разделитель не указан, пытаемся угадать
      if (is.null(sep)) {
        sep <- private$guessSeparator(filepath)  # теперь может бросить ошибку
      }
      
      df <- tryCatch({
        read.csv(filepath, sep = sep, stringsAsFactors = stringsAsFactors,
                       na.strings = c("", "NA", "N/A", "null", "NULL"))
      }, error = function(e) stop("Ошибка при чтении CSV: ", e$message))
      
      if (nrow(df) == 0) stop("Файл не содержит данных.")
      
      return(list(data = df, sep = sep))   # возвращаем и данные, и разделитель
    }
  )
)
