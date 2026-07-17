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
    
    # ---- Регистрация ----
    observeEvent(input$register_btn, {
      res <- register_user(input$login_username, input$login_password)
      output$login_message <- renderPrint(cat(res$message))
      if (res$success) {
        auth <- authenticate_user(input$login_username, input$login_password)
        if (auth$success) {
          app_state$logged_in <- TRUE
          app_state$user_id <- auth$user_id
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
      }
    })
  })
}
