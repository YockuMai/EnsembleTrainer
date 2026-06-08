# R/data_loader.R
# Функциональный аналог DataLoader (без R6)

#' Автоматическое определение разделителя в CSV-файле
#' @param filepath путь к файлу
#' @return строка с разделителем (",", ";", "\t", "|")
#' @throws ошибка, если файл не найден, пуст или разделитель не определён
guess_separator <- function(filepath) {
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
  
  # Кандидаты-разделители
  seps <- c(",", ";", "\t", "|")
  # Подсчёт частоты в первых 5 строках
  counts <- sapply(seps, function(s) {
    sum(stringr::str_count(lines[1:min(5, length(lines))], stringr::fixed(s)))
  })
  
  if (max(counts) == 0) {
    stop("Не удалось определить разделитель. Пожалуйста, укажите его вручную.")
  }
  
  best_sep <- seps[which.max(counts)]
  return(best_sep)
}

#' Загрузка CSV-файла с автоматическим определением разделителя
#' @param filepath путь к CSV-файлу
#' @param sep разделитель (если NULL – определяется автоматически)
#' @param stringsAsFactors преобразовывать строки в факторы? (по умолчанию FALSE)
#' @return список с двумя элементами: data (data.frame) и sep (использованный разделитель)
#' @throws ошибка при проблемах с файлом или чтением
load_csv <- function(filepath, sep = NULL, stringsAsFactors = FALSE) {
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
  
  # Определяем разделитель, если не указан
  if (is.null(sep)) {
    sep <- guess_separator(filepath)
  }
  
  # Чтение файла
  df <- tryCatch(
    read.csv(filepath, sep = sep, stringsAsFactors = stringsAsFactors,
             na.strings = c("", "NA", "N/A", "null", "NULL")),
    error = function(e) stop("Ошибка при чтении CSV: ", e$message)
  )
  
  if (nrow(df) == 0) {
    stop("Файл не содержит данных.")
  }
  
  return(list(data = df, sep = sep))
}