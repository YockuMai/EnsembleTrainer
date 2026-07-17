# modules/researchModules/predictionModule.R
# Использует predict_with_model() из R/model_trainers.R

predictionUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        width = 4,
        wellPanel(
          h4("Выберите модель"),
          uiOutput(ns("model_selector")),
          br(),
          actionButton(ns("predict_btn"), "Предсказать", class = "btn-primary btn-lg"),
          br(), br(),
          verbatimTextOutput(ns("prediction_result"))
        )
      ),
      column(
        width = 8,
        wellPanel(
          h4("Введите значения признаков"),
          uiOutput(ns("inputs_panel"))
        )
      )
    )
  )
}

predictionServer <- function(id, session_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---- Данные для полей ввода ----
    prediction_data <- reactive({
      df <- NULL
      if (!is.null(session_data$preprocess_path) && file.exists(session_data$preprocess_path)) {
        df <- load_data_frame(session_data$preprocess_path)
      } else if (!is.null(session_data$original_data_path) && file.exists(session_data$original_data_path)) {
        df <- load_data_frame(session_data$original_data_path)
      }
      df
    }) %>% bindEvent(
      session_data$original_data_path,
      session_data$preprocess_path,
      ignoreNULL = FALSE
    )
    
    target_var <- reactive({
      params <- session_data$model_params
      if (is.null(params)) return(NULL)
      params$target_var
    })
    
    # ---- Динамические поля ввода ----
    output$inputs_panel <- renderUI({
      df <- prediction_data()
      target <- target_var()
      if (is.null(df) || is.null(target)) {
        return(div("Данные не загружены или не выбрана целевая переменная"))
      }
      
      feature_cols <- setdiff(names(df), target)
      if (length(feature_cols) == 0) {
        return(div("Нет признаков для ввода"))
      }
      
      input_list <- lapply(feature_cols, function(col) {
        if (is.factor(df[[col]]) || is.character(df[[col]])) {
          choices <- if (is.factor(df[[col]])) levels(df[[col]]) else sort(unique(df[[col]]))
          selectInput(
            inputId = ns(paste0("input_", col)),
            label = col,
            choices = choices,
            selected = choices[1]
          )
        } else {
          numericInput(
            inputId = ns(paste0("input_", col)),
            label = col,
            value = NA,
            step = "any"
          )
        }
      })
      do.call(tagList, input_list)
    })
    
    # ---- Выбор модели ----
    output$model_selector <- renderUI({
      models <- session_data$trained_models
      if (is.null(models) || length(models) == 0) {
        return(div("Нет обученных моделей"))
      }
      model_names <- names(models)
      selectInput(
        inputId = ns("selected_model"),
        label = "Модель",
        choices = model_names,
        selected = model_names[1]
      )
    })
    
    # ---- Прогнозирование ----
    observeEvent(input$predict_btn, {
      df <- prediction_data()
      target <- target_var()
      if (is.null(df) || is.null(target)) {
        showNotification("Нет данных для прогнозирования", type = "warning")
        return()
      }
      
      selected_model_id <- input$selected_model
      if (is.null(selected_model_id)) {
        showNotification("Выберите модель", type = "warning")
        return()
      }
      
      models <- session_data$trained_models
      if (is.null(models) || !(selected_model_id %in% names(models))) {
        showNotification("Выбранная модель не найдена", type = "error")
        return()
      }
      model_meta <- models[[selected_model_id]]
      req(model_meta$path)
      
      # Загружаем модель
      mod <- tryCatch({
        load_model(model_meta$path)
      }, error = function(e) {
        showNotification(paste("Ошибка загрузки модели:", e$message), type = "error")
        return(NULL)
      })
      req(mod)
      
      # Собираем значения признаков
      feature_cols <- setdiff(names(df), target)
      if (length(feature_cols) == 0) {
        showNotification("Нет признаков для прогнозирования", type = "warning")
        return()
      }
      
      # Создаём строку с данными
      new_row <- list()
      for (col in feature_cols) {
        val <- input[[paste0("input_", col)]]
        if (is.null(val) || (is.character(val) && val == "")) {
          val <- NA
        }
        if (is.factor(df[[col]])) {
          new_row[[col]] <- safe_as_factor(val, levels(df[[col]]))
        } else if (is.numeric(df[[col]])) {
          new_row[[col]] <- safe_as_numeric(val)
        } else {
          new_row[[col]] <- as.character(val)
        }
      }
      new_row <- as.data.frame(new_row, stringsAsFactors = FALSE)
      
      # Предсказание через единый движок
      pred_result <- tryCatch({
        predict_with_model(mod, new_row, selected_model_id, model_meta$params)
      }, error = function(e) {
        showNotification(paste("Ошибка предсказания:", e$message), type = "error")
        return(NULL)
      })
      req(pred_result)
      
      # Формируем вывод
      output_text <- paste("Предсказанный класс:", pred_result$class)
      if (!is.null(pred_result$prob)) {
        if (is.matrix(pred_result$prob) && nrow(pred_result$prob) == 1) {
          prob_vec <- pred_result$prob[1, ]
          prob_text <- paste(names(prob_vec), round(prob_vec, 3), sep = ": ", collapse = ", ")
          output_text <- paste0(output_text, "\nВероятности: ", prob_text)
        } else if (is.numeric(pred_result$prob) && length(pred_result$prob) == 1) {
          output_text <- paste0(output_text, "\nВероятность: ", round(pred_result$prob, 3))
        }
      }
      output$prediction_result <- renderText(output_text)
      
      rm(mod); gc()
    })
    
  })
}