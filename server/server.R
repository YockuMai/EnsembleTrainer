source("server/loadDataServer.R")
source("server/preprocessServer.R")
source("R/sessionManager.R")

#plan(multisession)

server <- function(input, output, session) {

  # Создаем session_manager один раз на всю сессию
  session_manager <- SessionManager$new(session)

  # Загружаем сессионные данные при запуске
  session_data <- reactiveVal(list())

  observe({
    tryCatch({
      session_data(session_manager$load_session_data())
    }, error = function() {
      # Если не удалось загрузить, оставляем пустым
      session_data(list())
    })
  })

  # Загрузка данных
  loadDataServer("load", session_data)

  # Предобработка данных
  preprocessServer("preprocess", session_data)

  # Периодическое сохранение каждые 30 секунд
  #TODO: Изменить вреся сохранения сессионных файлов
  observe({
    invalidateLater(1000, session)
    data_to_save <- session_data()
    session_manager$save_session_data(data_to_save)
  })
}
