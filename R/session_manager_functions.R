# R/session_functions.R
# Функциональный аналог SessionManager (без R6)

# ---- Куки ----

#' Получить значение куки по имени
get_cookie <- function(session, name) {
  tryCatch(
    cookies::get_cookie(cookie_name = name, session = session),
    error = function(e) NULL
  )
}

#' Установить куку
set_cookie <- function(session, name, value, expires = NULL, path = NULL, domain = NULL, same_site = NULL) {
  tryCatch(
    cookies::set_cookie(
      cookie_name = name,
      cookie_value = value,
      expiration = expires,
      path = path,
      domain = domain,
      same_site = same_site,
      session = session
    ),
    error = function(e) warning("Error setting cookie: ", e$message)
  )
  invisible(value)
}

#' Удалить куку
remove_cookie <- function(session, name, path = NULL, domain = NULL) {
  cookies::remove_cookie(cookie_name = name, path = path, domain = domain, session = session)
}

#' Получить или создать user_id из куки
get_user_id <- function(session) {
  uid <- get_cookie(session, "user_id")
  if (is.null(uid) || uid == "") {
    uid <- paste0("user_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(9999, 1))
    set_cookie(session, "user_id", uid)
  }
  uid
}

# ---- Работа с директорией сессии ----

#' Путь к директории сессии пользователя
get_session_dir <- function(user_id) {
  file.path("session_data", user_id)
}

#' Создать директорию сессии, если её нет
ensure_session_dir <- function(user_id) {
  dir_path <- get_session_dir(user_id)
  dir.create(dir_path, showWarnings = FALSE, recursive = TRUE)
  dir_path
}

# ---- Сохранение и загрузка сессионных данных (RDS) ----

#' Сохранить сессионные данные (список) в директорию пользователя
#' @param session_data именованный список с данными
#' @param session объект Shiny session (для получения user_id)
save_session_data <- function(session_data, session) {
  user_id <- get_user_id(session)
  session_dir <- ensure_session_dir(user_id)
  
  # Получаем список уже существующих .rds файлов
  old_files <- list.files(session_dir, pattern = "\\.rds$", full.names = FALSE)
  
  # Сохраняем новые данные
  for (data_type in names(session_data)) {
    value <- session_data[[data_type]]
    rds_file <- paste0(data_type, ".rds")
    file_path <- file.path(session_dir, rds_file)
    
    if (is.null(value) || (is.list(value) && length(value) == 0)) {
      # Удаляем файл, если данные пустые
      if (file.exists(file_path)) file.remove(file_path)
    } else {
      # Сохраняем через временный файл для атомарности
      tmp <- tempfile(fileext = ".rds")
      saveRDS(value, tmp)
      file.copy(tmp, file_path, overwrite = TRUE)
      unlink(tmp)
    }
    # Убираем из списка старых файлов, чтобы потом удалить лишние
    old_files <- setdiff(old_files, rds_file)
  }
  
  # Удаляем файлы, которые были в старой сессии, но отсутствуют в новом session_data
  for (old in old_files) {
    file.remove(file.path(session_dir, old))
  }
  
  invisible(TRUE)
}

#' Загрузить все сессионные данные из директории пользователя
#' @param session объект Shiny session
#' @return список с загруженными данными (имена = имена .rds файлов без расширения)
load_session_data <- function(session) {
  user_id <- get_user_id(session)
  session_dir <- get_session_dir(user_id)
  session_data <- list()
  
  if (dir.exists(session_dir)) {
    rds_files <- list.files(session_dir, pattern = "\\.rds$", full.names = FALSE)
    for (file in rds_files) {
      data_type <- sub("\\.rds$", "", file)
      file_path <- file.path(session_dir, file)
      tryCatch({
        session_data[[data_type]] <- readRDS(file_path)
      }, error = function(e) {
        warning("Failed to load session file: ", file, " - ", e$message)
      })
    }
  }
  
  session_data
}

# ---- Дополнительные утилиты (сохранение/загрузка произвольных файлов) ----

#' Сохранить произвольный файл в сессию пользователя (не RDS)
#' @param name имя файла (относительно директории сессии)
#' @param content либо путь к исходному файлу (если binary = TRUE), либо текст (если binary = FALSE)
#' @param binary если TRUE, копирует файл из content; если FALSE, записывает content как текст
#' @param session объект Shiny session
#' @return путь к сохранённому файлу
save_session_file <- function(name, content, binary = FALSE, session) {
  user_id <- get_user_id(session)
  session_dir <- ensure_session_dir(user_id)
  target_path <- file.path(session_dir, name)
  dir.create(dirname(target_path), showWarnings = FALSE, recursive = TRUE)
  
  if (binary) {
    if (!file.exists(content)) stop("Source file does not exist: ", content)
    file.copy(content, target_path, overwrite = TRUE)
  } else {
    writeLines(as.character(content), target_path)
  }
  
  target_path
}

#' Загрузить файл из сессии пользователя
#' @param name имя файла
#' @param binary если TRUE, возвращает путь к файлу; если FALSE, возвращает содержимое как вектор строк
#' @param session объект Shiny session
load_session_file <- function(name, binary = FALSE, session) {
  user_id <- get_user_id(session)
  file_path <- file.path(get_session_dir(user_id), name)
  if (!file.exists(file_path)) return(NULL)
  
  if (binary) {
    return(file_path)
  } else {
    return(readLines(file_path, warn = FALSE))
  }
}

#' Проверить существование файла в сессии
has_session_file <- function(name, session) {
  user_id <- get_user_id(session)
  file_path <- file.path(get_session_dir(user_id), name)
  file.exists(file_path)
}

#' Удалить файл из сессии
remove_session_file <- function(name, session) {
  user_id <- get_user_id(session)
  file_path <- file.path(get_session_dir(user_id), name)
  if (file.exists(file_path)) file.remove(file_path)
}

#' Очистить все файлы сессии пользователя
clear_session_files <- function(session) {
  user_id <- get_user_id(session)
  session_dir <- get_session_dir(user_id)
  if (dir.exists(session_dir)) {
    unlink(list.files(session_dir, full.names = TRUE), recursive = TRUE)
  }
  invisible(TRUE)
}