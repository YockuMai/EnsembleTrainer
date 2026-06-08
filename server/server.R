source("server/loadDataServer.R")
source("server/preprocessServer.R")
source("R/session_manager_functions.R")   # функциональные функции для работы с сессией

# plan(multisession)  # раскомментировать при необходимости

create_server <- function() {
  function(input, output, session) {
    
    # Реактивное хранилище данных сессии
    session_data <- reactiveValues()
    
    # Восстановление сессии при старте (один раз)
    observeEvent(TRUE, {
      restore_data <- load_session_data(session)   # загружаем все .rds из директории пользователя
      if (length(restore_data) > 0) {
        for (nm in names(restore_data)) {
          session_data[[nm]] <- restore_data[[nm]]
        }
      }
    }, once = TRUE)
    
    # Модуль загрузки данных (использует session_data$original_data)
    loadDataServer("load", session_data)
    
    # Модуль предобработки (использует session_data$original_data и session_data$preprocess_obj)
    preprocessServer("preprocess", session_data)
    
    # Автосохранение состояния сессии с debounce (2 секунды без изменений)
    debounced_data <- reactive({
      reactiveValuesToList(session_data)
    }) %>% debounce(2000)
    
    observeEvent(debounced_data(), {
      save_session_data(debounced_data(), session)   # сохраняем всё состояние
    }, ignoreInit = TRUE)
    
    # Сохранение при завершении сессии (на всякий случай)
    onStop(function() {
      isolate({
        final_state <- reactiveValuesToList(session_data)
        save_session_data(final_state, session)
      })
    })
  }
}