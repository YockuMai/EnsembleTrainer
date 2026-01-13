SessionManager <- R6::R6Class("SessionManager",
private = list(
  session = NULL,
  user_id = NULL,

  # Получить куки по имени
  get_cookie = function(name) {
    tryCatch(
      cookies::get_cookie(name, session = private$session),
      error = function(e) NULL
    )
  },

  # Установить куки
  set_cookie = function(name, value, expires = NULL, path = NULL, domain = NULL, secure = NULL, same_site = NULL) {
    tryCatch(
      cookies::set_cookie(
        name = name,
        value = value,
        expires = expires,
        path = path,
        domain = domain,
        secure = secure,
        same_site = same_site,
        session = private$session
      ),
      error = function(e) {}
    )
  },

  # Удалить куки
  remove_cookie = function(name, path = NULL, domain = NULL) {
    cookies::remove_cookie(name = name, path = path, domain = domain, session = private$session)
  },

  # Получить или создать user_id
  get_user_id = function() {
    user_id <- private$get_cookie("user_id")
    if (is.null(user_id) || user_id == "") {
      user_id <- paste0("user_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(9999, 1))
      private$set_cookie("user_id", user_id)
    }
    return(user_id)
  },

  # Получить путь к директории сессии пользователя
  get_session_dir = function() {
    file.path("session_data", private$user_id)
  },

  # Убедиться, что директория сессии существует
  ensure_session_dir = function() {
    session_dir <- private$get_session_dir()
    dir.create(session_dir, showWarnings = FALSE, recursive = TRUE)
    return(session_dir)
  },

  # Сохранить файл сессии
  save_session_file = function(name, content, binary = FALSE) {
    session_dir <- private$ensure_session_dir()
    file_path <- file.path(session_dir, name)

    dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)

    if (binary) {
      # Для бинарных файлов (например, копирование загруженного CSV)
      if (file.exists(content)) {
        file.copy(content, file_path, overwrite = TRUE)
      } else {
        stop("Source file does not exist for binary save")
      }
    } else {
      # Для текстовых файлов
      writeLines(as.character(content), file_path)
    }

    return(file_path)
  },

  # Загрузить файл сессии
  load_session_file = function(name, binary = FALSE) {
    session_dir <- private$get_session_dir()
    file_path <- file.path(session_dir, name)

    if (!file.exists(file_path)) {
      return(NULL)
    }

    if (binary) {
      # Для бинарных файлов возвращаем путь
      return(file_path)
    } else {
      # Для текстовых файлов возвращаем содержимое
      return(readLines(file_path, warn = FALSE))
    }
  },

  # Проверить существование файла сессии
  has_session_file = function(name) {
    session_dir <- private$get_session_dir()
    file_path <- file.path(session_dir, name)
    return(file.exists(file_path))
  },

  # Удалить файл сессии
  remove_session_file = function(name) {
    session_dir <- private$get_session_dir()
    file_path <- file.path(session_dir, name)

    if (file.exists(file_path)) {
      file.remove(file_path)
      return(TRUE)
    }
    return(FALSE)
  },

  # Получить список файлов сессии
  list_session_files = function() {
    session_dir <- private$get_session_dir()
    if (dir.exists(session_dir)) {
      return(list.files(session_dir))
    }
    return(character(0))
  },

  # Очистить все файлы сессии
  clear_session_files = function() {
    session_dir <- private$get_session_dir()
    if (dir.exists(session_dir)) {
      files <- list.files(session_dir, full.names = TRUE)
      if (length(files) > 0) {
        unlink(files, recursive = TRUE)
      }
      return(TRUE)
    }
    return(FALSE)
  }
),

  public = list(
    initialize = function(session) {
      private$session <- session
    },

    # Инициализировать user_id (вызывать в reactive контексте)
    init_user_id = function() {
      private$user_id <- private$get_user_id()
    },

    # Загрузить все сессионные данные пользователя
    load_session_data = function() {
      session_data <- list()

      # Загрузка оригинальных данных
      if (private$has_session_file("original_data.rds")) {
        session_data$original_data <- readRDS(private$load_session_file("original_data.rds", binary = TRUE))
      } else {
        session_data$original_data <- NULL
      }

      # Загрузка предобработанных данных
      if (private$has_session_file("processed_data.rds")) {
        session_data$processed_data <- readRDS(private$load_session_file("processed_data.rds", binary = TRUE))
      } else {
        session_data$processed_data <- NULL
      }

      # Загрузка моделей
      if (private$has_session_file("models.rds")) {
        session_data$models <- readRDS(private$load_session_file("models.rds", binary = TRUE))
      } else {
        session_data$models <- list()
      }

      # Загрузка параметров моделей
      if (private$has_session_file("model_params.rds")) {
        session_data$model_params <- readRDS(private$load_session_file("model_params.rds", binary = TRUE))
      } else {
        session_data$model_params <- list()
      }

      return(session_data)
    },

    # Сохранить сессионные данные пользователя
    save_session_data = function(session_data) {
      # Сохранение оригинальных данных
      if (!is.null(session_data$original_data)) {
        temp_file <- tempfile(fileext = ".rds")
        saveRDS(session_data$original_data, temp_file)
        private$save_session_file("original_data.rds", temp_file, binary = TRUE)
        unlink(temp_file)
      } else {
        private$remove_session_file("original_data.rds")
      }

      # Сохранение предобработанных данных
      if (!is.null(session_data$processed_data)) {
        temp_file <- tempfile(fileext = ".rds")
        saveRDS(session_data$processed_data, temp_file)
        private$save_session_file("processed_data.rds", temp_file, binary = TRUE)
        unlink(temp_file)
      } else {
        private$remove_session_file("processed_data.rds")
      }

      # Сохранение моделей
      if (!is.null(session_data$models) && length(session_data$models) > 0) {
        temp_file <- tempfile(fileext = ".rds")
        saveRDS(session_data$models, temp_file)
        private$save_session_file("models.rds", temp_file, binary = TRUE)
        unlink(temp_file)
      } else {
        private$remove_session_file("models.rds")
      }

      # Сохранение параметров моделей
      if (!is.null(session_data$model_params) && length(session_data$model_params) > 0) {
        temp_file <- tempfile(fileext = ".rds")
        saveRDS(session_data$model_params, temp_file)
        private$save_session_file("model_params.rds", temp_file, binary = TRUE)
        unlink(temp_file)
      } else {
        private$remove_session_file("model_params.rds")
      }
    }
  )
)
