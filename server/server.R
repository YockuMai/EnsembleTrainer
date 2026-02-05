source("server/loadDataServer.R")
source("server/preprocessServer.R")
source("R/sessionManager.R")

#plan(multisession)

server <- function(input, output, session) {
  # Инициализация сессии
  session_manager <- SessionManager$new(session)
  session_data <- reactiveValues()
  
  observeEvent(TRUE, {

    restore_data <- session_manager$load_session_data()
    if (length(restore_data) > 0) {
      for (nm in names(restore_data)) {
        session_data[[nm]] <- restore_data[[nm]]
      }
    }

  }, once = TRUE)

  # Модуль загрузки данных
  loadDataServer("load", session_data)

  # Модуль предобработки данных
  preprocessServer("preprocess", session_data)

# Отдельный observe для сохранения с задержкой
  # Создаем реактивку с debounce
  debounced_data <- reactive({
    reactiveValuesToList(session_data)
  }) %>% debounce(2000)  # 2 секунды после последнего изменения
  
  # Сохраняем при изменении дебаунс-версии
  observeEvent(debounced_data(), {
    session_manager$save_session_data(debounced_data())
  }, ignoreInit = TRUE)



  onStop(function() { isolate({ session_manager$save_session_data(reactiveValuesToList(session_data)) }) })
}
