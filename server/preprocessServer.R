source("R/preprocessData.R")

preprocessServer <- function(id, session_data) {
  moduleServer(id, function(input, output, session) {
    preprocessData <- PreprocessData$new()

    # Инициализация processed_data копией original_data
    observe({
      currentData <- session_data()
      if (!is.null(currentData$original_data) && is.null(currentData$processed_data)) {
        currentData$processed_data <- currentData$original_data
        session_data(currentData)
      }
    })

    # Обновление списка столбцов для удаления
    observeEvent(input$update_columns, {
      data <- session_data()$original_data
      req(data)

      updateCheckboxGroupInput(session, "columns_to_remove",
                              choices = names(data),
                              selected = character(0))
    })

    # Показ статистики исходных данных
    output$original_summary <- renderUI({
      data <- session_data()$processed_data
      if (is.null(data)) {
        return(div(style = "color: red;", "Данные не загружены"))
      }

      num_rows <- nrow(data)
      num_cols <- ncol(data)
      num_missing <- sum(is.na(data))
      missing_pct <- round(num_missing / (num_rows * num_cols) * 100, 2)

      div(
        p(strong("Размер данных:"), paste(num_rows, "строк ×", num_cols, "столбцов")),
        p(strong("Пропущенные значения:"), paste(num_missing, "(", missing_pct, "%)")),
        p(strong("Типы столбцов:")),
        tags$ul(
          lapply(names(data), function(col) {
            col_type <- class(data[[col]])[1]
            num_na <- sum(is.na(data[[col]]))
            tags$li(paste(col, ":", col_type, "(NA:", num_na, ")"))
          })
        )
      )
    })

    # Таблица исходных данных
    output$original_data_table <- DT::renderDT({
      data <- session_data()$original_data
      req(data)

      DT::datatable(
        head(data, 100),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          searching = FALSE
        ),
        rownames = FALSE
      )
    })

    # Показ статистики обработанных данных
    output$processed_summary <- renderUI({
      data <- session_data()$processed_data
      if (is.null(data)) {
        return(div(style = "color: gray;", "Предобработка не применена"))
      }

      num_rows <- nrow(data)
      num_cols <- ncol(data)
      num_missing <- sum(is.na(data))
      missing_pct <- round(num_missing / (num_rows * num_cols) * 100, 2)

      div(
        p(strong("Размер данных:"), paste(num_rows, "строк ×", num_cols, "столбцов")),
        p(strong("Пропущенные значения:"), paste(num_missing, "(", missing_pct, "%)")),
        p(strong("Типы столбцов:")),
        tags$ul(
          lapply(names(data), function(col) {
            col_type <- class(data[[col]])[1]
            num_na <- sum(is.na(data[[col]]))
            tags$li(paste(col, ":", col_type, "(NA:", num_na, ")"))
          })
        )
      )
    })

    # Таблица обработанных данных
    output$processed_data_table <- DT::renderDT({
      data <- session_data()$processed_data
      req(data)

      DT::datatable(
        head(data, 100),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          searching = FALSE
        ),
        rownames = FALSE
      )
    })

    # Применение предобработки
    observeEvent(input$apply_preprocessing, {
      data <- session_data()$original_data
      req(data)

      # Определяем параметры предобработки
      handle_missing <- input$handle_missing
      missing_method <- if (handle_missing) input$missing_method else NULL

      handle_outliers <- input$handle_outliers
      outlier_method <- if (handle_outliers) input$outlier_method else NULL
      iqr_multiplier <- if (handle_outliers) input$iqr_multiplier else NULL

      scaling_method <- input$scaling_method
      normalize <- scaling_method == "normalize"
      standardize <- scaling_method == "standardize"

      columns_to_remove <- input$columns_to_remove
      if (is.null(columns_to_remove) || length(columns_to_remove) == 0) {
        columns_to_remove <- NULL
      }

      # Применяем предобработку
      tryCatch({
        processed_data <- preprocessData$preprocess(
          data = data,
          handle_outliers = handle_outliers,
          outlier_method = outlier_method,
          iqr_multiplier = iqr_multiplier,
          normalize = normalize,
          standardize = standardize,
          handle_missing = handle_missing,
          missing_method = missing_method,
          remove_columns = columns_to_remove
        )

        # Сохраняем обработанные данные
        current_data <- session_data()
        current_data$processed_data <- processed_data
        session_data(current_data)

        # Показываем статус успеха
        output$preprocessing_status <- renderUI({
          div(style = "color: green;", "✓ Предобработка успешно применена")
        })

      }, error = function(e) {
        # Показываем ошибку
        output$preprocessing_status <- renderUI({
          div(style = "color: red;", paste("✗ Ошибка предобработки:", e$message))
        })
      })
    })

    # Сброс предобработки
    observeEvent(input$reset_preprocessing, {
      # Очищаем обработанные данные
      current_data <- session_data()
      current_data$processed_data <- NULL
      session_data(current_data)

      # Сбрасываем статус
      output$preprocessing_status <- renderUI({
        div(style = "color: blue;", "Предобработка сброшена")
      })
    })

    # Статистика пропущенных значений
    output$missing_stats <- renderUI({
      data <- session_data()$processed_data
      if (is.null(data)) {
        return(div(style = "color: gray;", "Данные не загружены"))
      }

      total_rows <- nrow(data)
      rows_with_na <- preprocessData$get_count_missing_rows(data)
      na_percentage <- round((rows_with_na / total_rows) * 100, 2)

      div(
        style = "margin-bottom: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
        p("Всего строк: ", strong(total_rows)),
        p("Строк с пропущенными значениями: ", strong(rows_with_na)),
        p("Процент строк с пропусками: ", strong(na_percentage, "%"))
      )
    })

    # Инициализация - обновляем список столбцов при загрузке данных
    observe({
      data <- session_data()$original_data
      if (!is.null(data)) {
        updateCheckboxGroupInput(session, "columns_to_remove",
                                choices = names(data),
                                selected = character(0))
      }
    })
  })
}
