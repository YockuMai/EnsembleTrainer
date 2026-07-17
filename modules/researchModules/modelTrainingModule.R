# =============================================================================
# Модуль обучения моделей (UI + server)
# Использует R/model_trainers.R и R/stacking_utils.R
# =============================================================================

modelTrainingUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        div(
          style = "margin-bottom: 20px;",
          actionButton(
            inputId = ns("train_btn"),
            label = "Обучить выбранные модели",
            icon = icon("play"),
            class = "btn-success btn-lg"
          ),
          actionButton(
            inputId = ns("clear_btn"),
            label = "Очистить результаты",
            icon = icon("trash"),
            class = "btn-warning"
          )
        ),
        div(
          style = "margin-top: 20px;",
          uiOutput(ns("results_output"))
        )
      )
    )
  )
}

# -----------------------------------------------------------------------------
# Серверная часть модуля обучения
# -----------------------------------------------------------------------------

modelTrainingServer <- function(id, session_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---- Инициализация полей в session_data ----
    observe({
      if (is.null(session_data$trained_models)) {
        session_data$trained_models <- list()
      }
      if (is.null(session_data$training_results)) {
        session_data$training_results <- NULL
      }
    })
    
    # ---- Автоматическая очистка моделей при удалении данных ----
    observeEvent(c(session_data$original_data_path, session_data$preprocess_path), {
      if (is.null(session_data$original_data_path) && is.null(session_data$preprocess_path)) {
        if (length(session_data$trained_models) > 0) {
          for (meta in session_data$trained_models) {
            if (file.exists(meta$path)) file.remove(meta$path)
          }
          session_data$trained_models <- list()
          session_data$training_results <- NULL
          user_id <- session_data$user_id
          if (!is.null(user_id)) {
            save_user_data(user_id, session_data)
          }
          showNotification("Модели и результаты очищены в связи с удалением данных", type = "message")
        }
      }
    }, ignoreNULL = FALSE)
    
    # ---- Формирование строки параметров для таблицы ----
    format_params_string <- function(model_id, params_list) {
      if (model_id == "stack") {
        p <- params_list$params$stack
        paste0("base_models = ", paste(p$base_models, collapse = ", "),
               "; meta_model = ", p$meta_model)
      } else {
        p <- params_list$params[[model_id]]
        paste(names(p), unlist(p), sep = "=", collapse = "; ")
      }
    }
    
    # ---- Обработчик кнопки "Обучить" ----
    observeEvent(input$train_btn, {
      user_id <- session_data$user_id
      req(user_id)
      
      # Загружаем данные
      df <- NULL
      if (!is.null(session_data$preprocess_path) && file.exists(session_data$preprocess_path)) {
        df <- load_data_frame(session_data$preprocess_path)
      } else if (!is.null(session_data$original_data_path) && file.exists(session_data$original_data_path)) {
        df <- load_data_frame(session_data$original_data_path)
      }
      req(df, session_data$model_params)
      
      params_list <- session_data$model_params
      target <- params_list$target_var
      train_ratio <- params_list$train_ratio
      selected_models <- params_list$selected_models
      
      if (!is.factor(df[[target]])) {
        showNotification("Целевая переменная должна быть факторной", type = "error")
        return()
      }
      
      # Разделение на train/test
      set.seed(789)
      train_idx <- caret::createDataPartition(df[[target]], p = train_ratio, list = FALSE)
      train_data <- df[train_idx, ]
      test_data <- df[-train_idx, ]
      
      trained_models_meta <- list()
      results_all <- list()
      
      withProgress(message = "Обучение моделей...", value = 0, {
        for (i in seq_along(selected_models)) {
          model_id <- selected_models[i]
          incProgress(1 / length(selected_models), detail = paste("Модель:", model_id))
          
          params <- params_list$params[[model_id]]
          if (is.null(params)) {
            showNotification(paste("Параметры для модели", model_id, "не найдены"), type = "warning")
            next
          }
          
          result <- tryCatch({
            if (model_id == "stack") {
              # Стекинг — отдельная функция
              res <- train_stacking(params, train_data, test_data, target)
            } else {
              # Обычные модели — единый движок
              res <- train_single_model(model_id, params, train_data, test_data, target)
            }
            
            # Сохраняем модель на диск
            model_path <- save_model(res$model, user_id, model_id)
            
            trained_models_meta[[model_id]] <- list(
              path = model_path,
              metrics = res$metrics,
              params = params,
              class = res$class
            )
            
            # Формируем строку для таблицы результатов
            data.frame(
              Model = res$label,
              Accuracy = res$metrics["Accuracy"],
              Precision = res$metrics["Precision"],
              Recall = res$metrics["Recall"],
              F1 = res$metrics["F1"],
              AUC = res$metrics["AUC"],
              stringsAsFactors = FALSE
            )
          }, error = function(e) {
            showNotification(paste("Ошибка в модели", model_id, ":", e$message), type = "error")
            return(NULL)
          })
          
          if (!is.null(result)) {
            results_all[[model_id]] <- result
          }
          
          # Принудительная сборка мусора
          gc()
        }
      })
      
      # ---- Формирование итоговой таблицы ----
      if (length(results_all) > 0) {
        final_results <- do.call(rbind, results_all)
        rownames(final_results) <- NULL
        final_results$Params <- sapply(selected_models[seq_len(nrow(final_results))],
                                       format_params_string, params_list = params_list)
        
        session_data$trained_models <- trained_models_meta
        session_data$training_results <- final_results
        
        save_user_data(user_id, session_data)
        showNotification("Обучение завершено! Модели сохранены на диск.", type = "message")
      } else {
        showNotification("Не удалось обучить ни одну модель", type = "warning")
        session_data$trained_models <- list()
        session_data$training_results <- NULL
      }
      
      rm(df, train_data, test_data); gc()
    })
    
    # ---- Очистка результатов ----
    observeEvent(input$clear_btn, {
      if (!is.null(session_data$trained_models)) {
        for (meta in session_data$trained_models) {
          if (file.exists(meta$path)) file.remove(meta$path)
        }
      }
      session_data$trained_models <- list()
      session_data$training_results <- NULL
      showNotification("Результаты и модели очищены", type = "message")
    })
    
    # ---- Отображение результатов ----
    output$results_output <- renderUI({
      res <- session_data$training_results
      if (is.null(res)) {
        return(div(class = "alert alert-info", "Результаты обучения появятся здесь после нажатия кнопки."))
      }
      tagList(
        h4("Результаты обучения моделей"),
        DT::renderDT({
          DT::datatable(res, options = list(pageLength = 10, scrollX = TRUE, ordering = TRUE),
                        rownames = FALSE)
        })
      )
    })
    
  })
}