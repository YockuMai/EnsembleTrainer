# R/db_functions.R


get_db_conn <- function() {
  dbConnect(SQLite(), "ensemble_trainer.db")
}

init_db <- function() {
  conn <- get_db_conn()
  on.exit(dbDisconnect(conn))
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS users (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT UNIQUE NOT NULL,
      password_hash TEXT NOT NULL,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS session_metadata (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    user_id INTEGER NOT NULL,
    data_key TEXT NOT NULL,           -- Ключ данных (например, \"model\", \"predictions\")
    file_path TEXT NOT NULL,          -- Путь к файлу с данными
    file_size INTEGER,                -- Размер файла в байтах
    updated_at TEXT DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(id),
    UNIQUE(user_id, data_key)         -- Уникальность по user_id и data_key
    )
  ")
}

register_user <- function(username, password) {
  if (nchar(username) < 3) return(list(success = FALSE, message = "Логин слишком короткий"))
  if (nchar(password) < 4) return(list(success = FALSE, message = "Пароль слишком короткий"))
  conn <- get_db_conn()
  on.exit(dbDisconnect(conn))
  hash <- bcrypt::hashpw(password, bcrypt::gensalt())
  tryCatch({
    dbExecute(conn, "INSERT INTO users (username, password_hash) VALUES (?, ?)",
              params = list(username, hash))
    list(success = TRUE, message = "Пользователь зарегистрирован")
  }, error = function(e) {
    if (grepl("UNIQUE constraint", e$message)) {
      list(success = FALSE, message = "Пользователь с таким логином уже существует")
    } else {
      list(success = FALSE, message = paste("Ошибка БД:", e$message))
    }
  })
}

authenticate_user <- function(username, password) {
  conn <- get_db_conn()
  on.exit(dbDisconnect(conn))
  res <- dbGetQuery(conn, "SELECT id, password_hash FROM users WHERE username = ?",
                    params = list(username))
  if (nrow(res) == 0) {
    return(list(success = FALSE, user_id = NULL, message = "Неверный логин или пароль"))
  }
  if (bcrypt::checkpw(password, res$password_hash)) {
    return(list(success = TRUE, user_id = res$id[1], message = "Вход выполнен"))
  } else {
    return(list(success = FALSE, user_id = NULL, message = "Неверный логин или пароль"))
  }
}

# ----- Сохранение метаданных в SQLite (обновлено) -----
save_user_data <- function(user_id, session_data, base_path = "session_data") {
  if (is.null(user_id)) return(invisible(FALSE))
  
  conn <- get_db_conn()
  on.exit(dbDisconnect(conn))
  
  # Список ключей, которые мы сохраняем как файлы (пути или RDS)
  managed_keys <- c("original_data_path", "preprocess_path", 
                    "model_params", "training_results", "trained_models")
  
  # Удаляем старые записи для этих ключей (чтобы очистить удалённые)
  for (key in managed_keys) {
    dbExecute(conn, "DELETE FROM session_metadata WHERE user_id = ? AND data_key = ?",
              params = list(user_id, key))
  }
  
  # Теперь вставляем актуальные записи (остальной код без изменений)
  user_dir <- file.path(base_path, paste0("user_", user_id))
  if (!dir.exists(user_dir)) dir.create(user_dir, recursive = TRUE)
  
  for (key in names(session_data)) {
    value <- session_data[[key]]
    if (is.null(value)) next
    
    if (key %in% c("original_data_path", "preprocess_path") && is.character(value) && length(value) == 1) {
      if (file.exists(value)) {
        file_size <- file.info(value)$size
        dbExecute(conn,
          "INSERT INTO session_metadata (user_id, data_key, file_path, file_size, updated_at)
           VALUES (?, ?, ?, ?, CURRENT_TIMESTAMP)",
          params = list(user_id, key, value, file_size)
        )
      }
    } else if (key %in% c("model_params", "training_results", "trained_models")) {
      rds_path <- file.path(user_dir, paste0(key, ".rds"))
      saveRDS(value, rds_path)
      file_size <- file.info(rds_path)$size
      dbExecute(conn,
        "INSERT INTO session_metadata (user_id, data_key, file_path, file_size, updated_at)
         VALUES (?, ?, ?, ?, CURRENT_TIMESTAMP)",
        params = list(user_id, key, rds_path, file_size)
      )
    }
  }
  invisible(TRUE)
}

# ----- Загрузка метаданных из SQLite (обновлено) -----
load_user_data <- function(user_id, base_path = "session_data") {
  if (is.null(user_id)) return(NULL)
  
  conn <- get_db_conn()
  on.exit(dbDisconnect(conn))
  
  res <- dbGetQuery(
    conn,
    "SELECT data_key, file_path FROM session_metadata WHERE user_id = ?",
    params = list(user_id)
  )
  if (nrow(res) == 0) return(NULL)
  
  session_data <- list()
  for (i in 1:nrow(res)) {
    key <- res$data_key[i]
    path <- res$file_path[i]
    if (!file.exists(path)) next
    
    # Определяем тип по ключу
    if (key %in% c("original_data_path", "preprocess_path")) {
      # Это пути к FST файлам – сохраняем как строку
      session_data[[key]] <- path
    } else if (key %in% c("model_params", "training_results", "trained_models")) {
      # Это RDS файлы с метаданными
      session_data[[key]] <- readRDS(path)
    } else {
      # fallback: если файл .rds, пробуем загрузить
      if (grepl("\\.rds$", path)) {
        session_data[[key]] <- readRDS(path)
      } else {
        session_data[[key]] <- path
      }
    }
  }
  return(session_data)
}

# ----- Функции для работы с файлами датафреймов (FST) -----
save_data_frame <- function(df, user_id, key, base_path = "session_data") {
  user_dir <- file.path(base_path, paste0("user_", user_id))
  if (!dir.exists(user_dir)) dir.create(user_dir, recursive = TRUE)
  file_path <- file.path(user_dir, paste0(key, ".fst"))
  fst::write_fst(df, file_path, compress = 100)
  file_path
}

load_data_frame <- function(file_path) {
  if (!file.exists(file_path)) return(NULL)
  fst::read_fst(file_path)
}

# ----- Функции для работы с моделями -----
save_model <- function(model, user_id, model_id, base_path = "session_data") {
  user_dir <- file.path(base_path, paste0("user_", user_id), "models")
  if (!dir.exists(user_dir)) dir.create(user_dir, recursive = TRUE)
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  
  if (inherits(model, "xgb.Booster")) {
    file_path <- file.path(user_dir, paste0(model_id, "_", timestamp, ".xgb"))
    xgboost::xgb.save(model, file_path)
  } else {
    file_path <- file.path(user_dir, paste0(model_id, "_", timestamp, ".rds"))
    saveRDS(model, file_path, compress = "gzip")
  }
  file_path
}

load_model <- function(model_path) {
  if (!file.exists(model_path)) {
    stop("Файл модели не найден: ", model_path)
  }
  ext <- tools::file_ext(model_path)
  if (ext == "xgb") {
    return(xgboost::xgb.load(model_path))
  } else {
    return(readRDS(model_path))
  }
}

# ----- Очистка всех файлов пользователя и записей в БД -----
clear_user_files <- function(user_id, base_path = "session_data") {
  user_dir <- file.path(base_path, paste0("user_", user_id))
  if (dir.exists(user_dir)) {
    unlink(user_dir, recursive = TRUE)
  }
  conn <- get_db_conn()
  on.exit(dbDisconnect(conn))
  dbExecute(conn, "DELETE FROM session_metadata WHERE user_id = ?", params = list(user_id))
  invisible(TRUE)
}