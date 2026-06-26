source("R/db_functions.R")

source("modules/authModule.R")
source("modules/appModule.R")

source("modules/researchModules/mainResearchModule.R")
source("modules/researchModules/loadDataModule.R")
source("modules/researchModules/preprocessDataModule.R")
source("modules/researchModules/trainModelModule.R")

source("modules/learnModules/mainLearnModule.R")

create_ui <- function() {
  ui <- fluidPage(
    titlePanel("Ensemble Trainer"),
    uiOutput("main_ui")
  )
}

create_server <- function() {
  function(input, output, session) {
    init_db()
    
    app_state <- reactiveValues(
      logged_in = FALSE,
      user_id = NULL,
      session_data = reactiveValues()  # единый контейнер
    )
    
    output$main_ui <- renderUI({
      if (app_state$logged_in) {
        appUI("main")
      } else {
        authUI("auth")
      }
    })
    
    # ---- Модули (один раз) ----
    authServer("auth", app_state)
    
    appServer("main", app_state) 
    
    loadDataServer("load", app_state$session_data)
    preprocessServer("preprocess", app_state$session_data)
    trainModelServer("train", app_state$session_data)
    
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