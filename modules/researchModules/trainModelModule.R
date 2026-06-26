# R/trainModelModule.R
source("R/train_model_functions.R")

trainModelUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 3,
        h4("Выбор моделей"),
        uiOutput(ns("model_choices")),
        br(),
        h4("Разделение данных"),
        sliderInput(ns("train_ratio"), "Доля обучающей выборки",
                    min = 0.5, max = 0.95, value = 0.8, step = 0.05),
        selectInput(ns("target_variable"), "Целевая переменная (категориальная)",
                    choices = NULL),
        br(),
        actionButton(ns("train_btn"), "Обучить модели",
                     class = "btn-primary", style = "width: 100%;"),
        br(), br(),
        div(style = "background: #f5f5f5; padding: 10px; border-radius: 5px;",
            verbatimTextOutput(ns("train_status")))
      ),
      column(
        width = 9,
        h4("Настройки моделей"),
        uiOutput(ns("model_params_tabs"))
      )
    ),
    hr(),
    h4("Результаты обучения"),
    DT::dataTableOutput(ns("results_table")),
    br(),
    uiOutput(ns("results_plots"))
  )
}

trainModelServer <- function(id, session_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$model_choices <- renderUI({
      model_defs <- get_model_definitions()
      model_names <- names(model_defs)
      model_display_names <- sapply(model_defs, function(x) x$name)
      checkboxGroupInput(
        session$ns("selected_models"),
        label = NULL,
        choices = setNames(model_names, model_display_names),
        selected = model_names[1:min(2, length(model_names))]
      )
    })
    
    observe({
      data <- session_data$preprocess_obj
      if (is.null(data)) {
        updateSelectInput(session, "target_variable", choices = character(0))
      } else {
        factor_cols <- names(data)[vapply(data, is.factor, logical(1))]
        if (length(factor_cols) > 0) {
          updateSelectInput(session, "target_variable",
                            choices = factor_cols,
                            selected = factor_cols[1])
        } else {
          updateSelectInput(session, "target_variable", choices = character(0))
        }
      }
    })
    
    output$model_params_tabs <- renderUI({
      selected <- input$selected_models
      if (is.null(selected) || length(selected) == 0) {
        return(p("Выберите хотя бы одну модель для настройки"))
      }
      model_defs <- get_model_definitions()
      tabs <- lapply(selected, function(model_id) {
        def <- model_defs[[model_id]]
        tabPanel(
          title = def$name,
          create_param_ui(def, ns(paste0("params_", model_id)))
        )
      })
      do.call(tabsetPanel, tabs)
    })
    
    observeEvent(input$train_btn, {
      print("Кнопка 'Обучить' нажата")  # отладка
      
      data <- session_data$preprocess_obj
      if (is.null(data)) {
        showNotification("Данные отсутствуют. Загрузите и обработайте данные.", type = "error")
        return()
      }
      
      target <- input$target_variable
      if (is.null(target) || target == "") {
        showNotification("Выберите целевую переменную (категориальную).", type = "warning")
        return()
      }
      if (!is.factor(data[[target]])) {
        showNotification("Целевая переменная должна быть фактором.", type = "error")
        return()
      }
      if (length(unique(data[[target]])) < 2) {
        showNotification("Целевая переменная должна иметь хотя бы два уровня.", type = "error")
        return()
      }
      
      selected <- input$selected_models
      print(paste("selected_models:", selected))  # отладка
      if (is.null(selected) || length(selected) == 0) {
        showNotification("Выберите хотя бы одну модель.", type = "warning")
        return()
      }
      
      # Разделение данных
      split <- split_train_test(data, target, input$train_ratio)
      train_data <- split$train
      test_data <- split$test
      formula <- as.formula(paste(target, "~ ."))
      
      results <- list()
      trained_models <- list()
      model_defs <- get_model_definitions()
      
      withProgress(message = "Обучение моделей...", value = 0, {
        for (i in seq_along(selected)) {
          model_id <- selected[i]
          def <- model_defs[[model_id]]
          incProgress(1 / length(selected), detail = paste("Обучение:", def$name))
          
          param_values <- get_params_from_input(input, def, ns(paste0("params_", model_id)))
          print(paste("Параметры для", def$name, ":"))
          print(param_values)
          
          tryCatch({
            model <- def$train_func(formula, data = train_data, params = param_values)
            trained_models[[model_id]] <- list(model = model, name = def$name)
            
            if (model_id == "xgboost") {
              prob_test <- def$predict_func(model, test_data[, !names(test_data) %in% target],
                                            target_levels = levels(train_data[[target]]))
            } else {
              prob_test <- def$predict_func(model, test_data)
            }
            
            metrics <- calc_metrics(prob_test, test_data[[target]])
            results[[model_id]] <- data.frame(
              Модель = def$name,
              Accuracy = round(metrics$Accuracy, 4),
              Precision = round(metrics$Precision, 4),
              Recall = round(metrics$Recall, 4),
              F1 = round(metrics$F1, 4),
              AUC = round(metrics$AUC, 4),
              stringsAsFactors = FALSE
            )
          }, error = function(e) {
            print(paste("Ошибка при обучении", def$name, ":", e$message))
            results[[model_id]] <- data.frame(
              Модель = def$name,
              Accuracy = NA, Precision = NA, Recall = NA, F1 = NA, AUC = NA,
              Ошибка = e$message,
              stringsAsFactors = FALSE
            )
          })
        }
      })
      
      session_data$models <- list(
        trained = trained_models,
        metrics = do.call(rbind, results),
        train_idx = split$train_idx,
        test_idx = split$test_idx,
        target = target,
        formula = formula
      )
      
      output$results_table <- DT::renderDataTable({
        metrics <- session_data$models$metrics
        if (is.null(metrics)) return(NULL)
        DT::datatable(metrics, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
      })
      
      output$results_plots <- renderUI({
        metrics <- session_data$models$metrics
        if (is.null(metrics)) return(NULL)
        plotOutput(ns("metrics_plot"))
      })
      
      output$metrics_plot <- renderPlot({
        metrics <- session_data$models$metrics
        if (is.null(metrics)) return(NULL)
        metrics <- metrics[!is.na(metrics$Accuracy), ]
        if (nrow(metrics) == 0) return(plot.new())
        metric_cols <- c("Accuracy", "Precision", "Recall", "F1", "AUC")
        metrics_long <- tidyr::pivot_longer(metrics, cols = metric_cols,
                                            names_to = "Metric", values_to = "Value")
        ggplot2::ggplot(metrics_long, ggplot2::aes(x = Модель, y = Value, fill = Metric)) +
          ggplot2::geom_col(position = ggplot2::position_dodge()) +
          ggplot2::theme_minimal() +
          ggplot2::labs(title = "Сравнение метрик моделей", y = "Значение")
      })
      
      showNotification("Обучение завершено!", type = "message")
    })
    
    output$train_status <- renderText({
      if (is.null(session_data$models)) {
        return("Модели не обучены")
      }
      paste("Обучено моделей:", length(session_data$models$trained), "\n",
            "Дата:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    })
    
    observe({
      if (!is.null(session_data$models)) {
        output$results_table <- DT::renderDataTable({
          DT::datatable(session_data$models$metrics, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
        })
      }
    })
  })
}