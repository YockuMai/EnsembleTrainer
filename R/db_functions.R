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
    CREATE TABLE IF NOT EXISTS user_data (
      user_id INTEGER NOT NULL,
      data_key TEXT NOT NULL,
      data_value BLOB,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (user_id, data_key),
      FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
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

save_user_data <- function(user_id, data_list) {
  if (is.null(user_id)) return(invisible(FALSE))
  conn <- get_db_conn()
  on.exit(dbDisconnect(conn))
  
  for (key in names(data_list)) {
    value <- data_list[[key]]
    if (is.null(value)) {
      dbExecute(conn, "DELETE FROM user_data WHERE user_id = ? AND data_key = ?",
                params = list(user_id, key))
    } else {
      raw_val <- serialize(value, NULL, version = 2)
      # Используем dbSendStatement + dbBind для корректной передачи BLOB
      stmt <- dbSendStatement(conn, "
        INSERT INTO user_data (user_id, data_key, data_value, updated_at)
        VALUES (?, ?, ?, CURRENT_TIMESTAMP)
        ON CONFLICT(user_id, data_key) DO UPDATE SET
          data_value = excluded.data_value,
          updated_at = CURRENT_TIMESTAMP
      ")
      # ВАЖНО: BLOB передаём как список из одного raw вектора
      dbBind(stmt, list(user_id, key, list(raw_val)))
      dbClearResult(stmt)
    }
  }
  invisible(TRUE)
}

load_user_data <- function(user_id) {
  if (is.null(user_id)) return(list())
  conn <- get_db_conn()
  on.exit(dbDisconnect(conn))
  res <- dbGetQuery(conn, "SELECT data_key, data_value FROM user_data WHERE user_id = ?",
                    params = list(user_id))
  if (nrow(res) == 0) return(list())
  result <- list()
  for (i in 1:nrow(res)) {
    key <- res$data_key[i]
    raw_val <- res$data_value[[i]]   # уже raw вектор
    if (!is.null(raw_val) && length(raw_val) > 0) {
      result[[key]] <- unserialize(raw_val)
    } else {
      result[[key]] <- NULL
    }
  }
  result
}

clear_user_data <- function(user_id) {
  if (is.null(user_id)) return(invisible(FALSE))
  conn <- get_db_conn()
  on.exit(dbDisconnect(conn))
  dbExecute(conn, "DELETE FROM user_data WHERE user_id = ?", params = list(user_id))
  invisible(TRUE)
}