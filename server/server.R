source("R/db_functions.R")
source("server/loadDataServer.R")
source("server/preprocessServer.R")
library(dplyr)

create_server <- function() {
  function(input, output, session) {
    init_db()
    
    app_state <- reactiveValues(
      logged_in = FALSE,
      user_id = NULL,
      session_data = reactiveValues()  # единый контейнер
    )
    
    # ---- UI для логина ----
    output$login_ui <- renderUI({
      if (!app_state$logged_in) {
        tagList(
          textInput("login_username", "Логин"),
          passwordInput("login_password", "Пароль"),
          actionButton("login_btn", "Войти"),
          actionButton("register_btn", "Зарегистрироваться"),
          br(),
          verbatimTextOutput("login_message")
        )
      } else {
        tagList(
          div(style = "text-align: right; margin-bottom: 20px;",
              actionButton("logout_btn", "Выйти")
          ),
          tabsetPanel(type = "pills",
                      tabPanel("Теория",
                               lectionUI()
                      ),
                      tabPanel("Практика",
                               practicUI()
                      ),
                      tabPanel("Тестирование",
                               testUI()
                      )
          )
        )
      }
    })
    
    # Вспомогательная функция очистки session_data
    clear_session_data <- function() {
      isolate({
        keys <- names(reactiveValuesToList(app_state$session_data))
        for (key in keys) app_state$session_data[[key]] <- NULL
      })
    }
    
    # ---- Регистрация ----
    observeEvent(input$register_btn, {
      res <- register_user(input$login_username, input$login_password)
      output$login_message <- renderPrint(cat(res$message))
      if (res$success) {
        auth <- authenticate_user(input$login_username, input$login_password)
        if (auth$success) {
          app_state$logged_in <- TRUE
          app_state$user_id <- auth$user_id
          clear_session_data()
          saved_data <- load_user_data(app_state$user_id)
          for (nm in names(saved_data)) app_state$session_data[[nm]] <- saved_data[[nm]]
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
        clear_session_data()
        saved_data <- load_user_data(app_state$user_id)
        for (nm in names(saved_data)) app_state$session_data[[nm]] <- saved_data[[nm]]
      }
    })
    
    # ---- Выход ----
    observeEvent(input$logout_btn, {
      if (!is.null(app_state$user_id)) {
        isolate({
          current_data <- reactiveValuesToList(app_state$session_data)
          save_user_data(app_state$user_id, current_data)
        })
      }
      app_state$logged_in <- FALSE
      app_state$user_id <- NULL
      clear_session_data()
      showNotification("Вы вышли из системы", type = "message")
    })
    
    # ---- Модули (один раз) ----
    loadDataServer("load", app_state$session_data)
    preprocessServer("preprocess", app_state$session_data)
    
    # ---- Автосохранение ----
    debounced_data <- reactive({
      req(app_state$logged_in)
      reactiveValuesToList(app_state$session_data)
    }) %>% debounce(2000)
    
    observeEvent(debounced_data(), {
      req(app_state$logged_in, app_state$user_id)
      save_user_data(app_state$user_id, debounced_data())
    }, ignoreInit = TRUE)
    
    # ---- Сохранение при закрытии ----
    session$onSessionEnded(function() {
      if (isolate(app_state$logged_in) && !is.null(isolate(app_state$user_id))) {
        isolate({
          final_data <- reactiveValuesToList(app_state$session_data)
          save_user_data(app_state$user_id, final_data)
        })
      }
    })
  }
}