appUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(style = "display: flex; justify-content: flex-end; margin-bottom: 20px;",
        actionButton(ns("logout_btn"), "Выйти", class = "btn-danger")
    ),
    # Сюда рендерится контент в зависимости от режима
    uiOutput(ns("content_area"))
  )
}

appServer <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    current_mode <- reactiveVal(NULL)
    
    # Обработчики для статических кнопок
    observeEvent(input$mode_learning, {
      current_mode("learning")
    })
    
    observeEvent(input$mode_research, {
      current_mode("research")
    })
    
    observeEvent(input$back_to_modes, {
      current_mode(NULL)
    })
    
    # Рендерим контент в зависимости от режима
    output$content_area <- renderUI({
      mode <- current_mode()
      if (is.null(mode)) {
        # Ничего не показываем или приветствие
        div(style = "text-align: center; margin-top: 50px;",
            h3("Выберите режим работы"))
        # Кнопки выбора режима всегда присутствуют в DOM
        div(style = "display: flex; gap: 20px; justify-content: center; margin: 50px 0;",
            actionButton(ns("mode_learning"), "Режим обучения", 
                         style = "width: 200px; height: 100px; font-size: 24px;"),
            actionButton(ns("mode_research"), "Режим исследования",
                         style = "width: 200px; height: 100px; font-size: 24px;")
        )
      } else if (mode == "learning") {
        tagList(
          div(style = "margin-bottom: 20px;",
              actionButton(session$ns("back_to_modes"), "← Назад к выбору режима",
                           class = "btn-secondary")
          ),
          learnUI()
        )
      } else if (mode == "research") {
        tagList(
          div(style = "margin-bottom: 20px;",
              actionButton(session$ns("back_to_modes"), "← Назад к выбору режима",
                           class = "btn-secondary")
          ),
          practicUI()
        )
      }
    })
    
    # Обработка выхода
    observeEvent(input$logout_btn, {
      if (!is.null(app_state$user_id)) {
        isolate({
          current_data <- reactiveValuesToList(app_state$session_data)
          save_user_data(app_state$user_id, current_data)
        })
      }
      app_state$logged_in <- FALSE
      app_state$user_id <- NULL
      isolate({
        keys <- names(reactiveValuesToList(app_state$session_data))
        for (key in keys) app_state$session_data[[key]] <- NULL
      })
      showNotification("Вы вышли из системы", type = "message")
    })
  })
}