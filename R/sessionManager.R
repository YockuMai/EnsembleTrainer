SessionManager <- R6::R6Class("SessionManager",
private = list(
  session = NULL,
  user_id = NULL,

  # Получить куки по имени
  get_cookie = function(name) {
    tryCatch(
      cookies::get_cookie(cookie_name = name, session = private$session),
      error = function(e) NULL
    )
  },

  # Установить куки
  set_cookie = function(name, value, expires = NULL, path = NULL, domain = NULL, same_site = NULL) {
    tryCatch(
      cookies::set_cookie(
        cookie_name = name,
        cookie_value = value,
        expiration = expires,
        path = path,
        domain = domain,
        same_site = same_site,
        session = private$session
      ),
      error = function(e) cat("Error setting cookie:", e$message, "\n")
    )
  },

  # Удалить куки
  remove_cookie = function(name, path = NULL, domain = NULL) {
    cookies::remove_cookie(cookie_name = name, path = path, domain = domain, session = private$session)
  },

  # Получить или создать user_id
  get_user_id = function() {
    private$user_id <- private$get_cookie("user_id")
    if (is.null(private$user_id) || private$user_id == "") {
      private$user_id <- paste0("user_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(9999, 1))
      private$set_cookie("user_id", private$user_id)
    }
    return(private$user_id)
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
      private$user_id <- NULL
    },

    # Универсальный метод загрузки всех сессионных данных
    load_session_data = function() {
      if (is.null(private$user_id)) {
        private$get_user_id()
      }

      session_data <- list()
      session_dir <- private$get_session_dir()

      # Если директория сессии существует, сканируем все .rds файлы
      if (dir.exists(session_dir)) {
        rds_files <- list.files(session_dir, pattern = "\\.rds$", full.names = FALSE)

        for (file in rds_files) {
          # Убираем расширение .rds для получения имени типа данных
          data_type <- gsub("\\.rds$", "", file)
          file_path <- file.path(session_dir, file)

          tryCatch({
            session_data[[data_type]] <- readRDS(file_path)
          }, error = function(e) {
            warning(paste("Не удалось загрузить файл сессии:", file, "-", e$message))
          })
        }
      }

      return(session_data)
    },

    # Универсальный метод сохранения сессионных данных
    save_session_data = function(session_data) {
      if (is.null(private$user_id)) {
        private$get_user_id()
      }

      session_dir <- private$get_session_dir()

      # Создаем директорию, если не существует
      if (!dir.exists(session_dir)) {
        dir.create(session_dir, recursive = TRUE)
      }

      # Получаем список текущих файлов сессии
      current_files <- if (dir.exists(session_dir)) {
        list.files(session_dir, pattern = "\\.rds$", full.names = FALSE)
      } else {
        character(0)
      }

      # Сохраняем все элементы из session_data
      for (data_type in names(session_data)) {
        data_value <- session_data[[data_type]]

        # Проверяем, нужно ли сохранять этот элемент
        if (!is.null(data_value)) {
          # Для списков проверяем, что они не пустые
          if (is.list(data_value) && length(data_value) == 0) {
            # Удаляем пустые списки
            private$remove_session_file(paste0(data_type, ".rds"))
          } else {
            # Сохраняем данные
            temp_file <- tempfile(fileext = ".rds")
            tryCatch({
              saveRDS(data_value, temp_file)
              private$save_session_file(paste0(data_type, ".rds"), temp_file, binary = TRUE)
            }, error = function(e) {
              warning(paste("Не удалось сохранить сессионные данные:", data_type, "-", e$message))
            }, finally = {
              if (file.exists(temp_file)) {
                unlink(temp_file)
              }
            })
          }
        } else {
          # Удаляем файл, если данные NULL
          private$remove_session_file(paste0(data_type, ".rds"))
        }

        # Удаляем из списка текущих файлов, чтобы потом удалить оставшиеся
        current_files <- current_files[current_files != paste0(data_type, ".rds")]
      }

      # Удаляем файлы, которые больше не нужны (были в сессии, но больше не передаются)
      for (old_file in current_files) {
        private$remove_session_file(old_file)
      }
    }
  )
)
