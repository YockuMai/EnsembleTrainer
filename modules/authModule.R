authUI <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("login_username"), "Логин"),
    passwordInput(ns("login_password"), "Пароль"),
    actionButton(ns("login_btn"), "Войти"),
    actionButton(ns("register_btn"), "Зарегистрироваться"),
    br(),
    verbatimTextOutput(ns("login_message"))
  )
}

authServer <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    # Вспомогательная функция очистки session_data
    #clear_session_data <- function() {
    #  isolate({
    #    keys <- names(reactiveValuesToList(app_state$session_data))
    #    for (key in keys) app_state$session_data[[key]] <- NULL
    #  })
    #}
    
    # ---- Регистрация ----
    observeEvent(input$register_btn, {
      res <- register_user(input$login_username, input$login_password)
      output$login_message <- renderPrint(cat(res$message))
      if (res$success) {
        auth <- authenticate_user(input$login_username, input$login_password)
        if (auth$success) {
          app_state$logged_in <- TRUE
          app_state$user_id <- auth$user_id
          #clear_session_data()
        }
      }
    })
    
    # ---- Вход ----
    observeEvent(input$login_btn, {
      auth <- authenticate_user(input$login_username, input$login_password)
      output$login_message <- renderPrint(cat(auth$message))
      if (auth$success) {
        app_state$logged_in <- TRUE
        app_state$user_id <- auth$user_id
        #clear_session_data()
        #saved_data <- load_user_data(app_state$user_id)
        #for (nm in names(saved_data)) app_state$session_data[[nm]] <- saved_data[[nm]]
      }
    })
  }
  )}