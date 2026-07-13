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

save_user_data <- function(user_id, session_data, base_path = "session_data") {
  # Папка пользователя
  user_dir <- file.path(base_path, paste0("user_", user_id))
  if (!dir.exists(user_dir)) {
    dir.create(user_dir, recursive = TRUE)
  }
  
  conn <- get_db_conn()
  on.exit(dbDisconnect(conn))
  
  for (key in names(session_data)) {
    data_item <- session_data[[key]]
    #if (is.null(data_item)) next
    
    file_path <- file.path(user_dir, paste0(key, ".rds"))
    saveRDS(data_item, file_path)
    
    # Обновляем или вставляем запись в БД
    dbExecute(
      conn,
      "INSERT OR REPLACE INTO session_metadata (user_id, data_key, file_path, file_size, updated_at)
       VALUES (?, ?, ?, ?, CURRENT_TIMESTAMP)",
      params = list(user_id, key, file_path, file.info(file_path)$size)
    )
  }
  user_dir
}

load_user_data <- function(user_id, base_path = "session_data") {
  user_dir <- file.path(base_path, paste0("user_", user_id))
  if (!dir.exists(user_dir)) {
    return(NULL)  # нет данных
  }
  
  # Получаем список файлов .rds в папке
  rds_files <- list.files(user_dir, pattern = "\\.rds$", full.names = TRUE)
  if (length(rds_files) == 0) return(NULL)
  
  session_data <- list()
  for (file_path in rds_files) {
    key <- tools::file_path_sans_ext(basename(file_path))
    session_data[[key]] <- readRDS(file_path)
  }
  session_data
}

clear_user_data <- function(user_id) {
  if (is.null(user_id)) return(invisible(FALSE))
  conn <- get_db_conn()
  on.exit(dbDisconnect(conn))
  dbExecute(conn, "DELETE FROM user_data WHERE user_id = ?", params = list(user_id))
  invisible(TRUE)
}