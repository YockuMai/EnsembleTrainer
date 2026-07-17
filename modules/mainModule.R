source("R/db_functions.R")
source("R/utils.R")
source("R/model_trainers.R")
source("R/stacking_utils.R")

source("modules/authModule.R")
source("modules/appModule.R")

source("modules/researchModules/mainResearchModule.R")
source("modules/researchModules/loadDataModule.R")
source("modules/researchModules/preprocessDataModule.R")
source("modules/researchModules/modelParamsModule.R")
source("modules/researchModules/modelTrainingModule.R")
source("modules/researchModules/modelPredictionModule.R")

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

    # --- Добавляем наблюдение за входом ---
    observe({
      if (app_state$logged_in && !is.null(app_state$user_id)) {
        # Загружаем метаданные из SQLite
        saved_meta <- load_user_data(app_state$user_id)
        if (!is.null(saved_meta)) {
          for (nm in names(saved_meta)) {
            app_state$session_data[[nm]] <- saved_meta[[nm]]
          }
        }
        # Сохраняем user_id в session_data для использования в модулях
        app_state$session_data$user_id <- app_state$user_id
      } else {
        # При выходе очищаем session_data
        isolate({
          keys <- names(reactiveValuesToList(app_state$session_data))
          for (key in keys) app_state$session_data[[key]] <- NULL
        })
      }
    })
    
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
    modelParamsServer("model_params", app_state$session_data)
    modelTrainingServer("model_training", app_state$session_data)
    predictionServer("prediction", app_state$session_data)
    
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