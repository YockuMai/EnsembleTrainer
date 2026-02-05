source("R/preprocessData.R")

preprocessServer <- function(id, session_data) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    preprocess_obj <- reactiveVal(NULL)

    observe({
      if (!is.null(session_data$preprocess_obj) && is.null(preprocess_obj())) {
        preprocess_obj(session_data$preprocess_obj)   # восстановление
      } else if (!is.null(session_data$original_data) && is.null(preprocess_obj())) {
        preprocess_obj(PreprocessData$new(session_data$original_data))  # новый объект
      } else if (is.null(session_data$original_data)) {
        preprocess_obj(NULL)
      }
    })

    mutate_preprocess <- function(f) {
      obj <- preprocess_obj()
      if (is.null(obj)) return(NULL)

      f(obj)                 # мутируем объект

      # Форсируем обновление reactiveVal — создаём копию объекта
      preprocess_obj(obj$clone(deep = TRUE))  # <- лучше: клонируем объект
    }

    observeEvent(preprocess_obj(), {
      session_data$preprocess_obj <- preprocess_obj()
    }, ignoreNULL = FALSE)

    # Просмотр данных -> Просмотр
    output$data_overview <- DT::renderDataTable({
      #preprocess_trigger()
      obj <- preprocess_obj()
      req(obj)
      if (is.null(obj)) return(data.frame())
      datatable(
        obj$get_data(),
        options = list(
        pageLength = 10,  # Строк на странице
        scrollX = TRUE,    # Горизонтальная прокрутка
        searching = TRUE,  # Поиск
        ordering = TRUE,   # Сортировка
        language = list(
          search = "Поиск:",
          lengthMenu = "Показать _MENU_ записей",
          info = "Показаны _START_ до _END_ из _TOTAL_ записей",
          infoEmpty = "Нет данных",
          infoFiltered = "(отфильтровано из _MAX_ записей)",
          paginate = list(
            'first' = "Первая",
            'last' = "Последняя",
            'next' = "Следующая",
            'previous' = "Предыдущая"
          )
        )
      ),
        rownames = FALSE
      )
    })

    output$data_summary <- renderUI({
      obj <- preprocess_obj()
      req(obj)

      summ <- summary(obj$get_data())
      summ_text <- paste(capture.output(print(summ)), collapse = "\n")

      tags$pre(summ_text)
    })


    output$missing_info <- renderText({
      obj <- preprocess_obj()
      req(obj)

      stat <- obj$get_missing_statistic()

      paste0(
        "Всего строк: ", stat$rows,
        "\nСтрок с пропусками: ", stat$count,
        "\nПроцент: ", stat$percentage, "%"
      )
    })

    output$outliers_info <- renderText({
      obj <- preprocess_obj()
      req(obj)

      stat <- obj$get_outliers_statistic()

      if (stat$total_outliers == 0) {
        "Выбросы не обнаружены"
      } else {
        paste(
          "Всего выбросов:", stat$total_outliers,
          "\nКолонки:", paste(names(stat$outliers_by_column), collapse = ", ")
        )
      }
    })

    output$scaling_info <- renderText({
      obj <- preprocess_obj()
      req(obj)

      paste("Текущее масштабирование:", obj$get_scaling_type())
    })

    observe({
      obj <- preprocess_obj()
      if (is.null(obj)) return()

      updateCheckboxGroupInput(session, "numeric_cols_selected", choices = obj$get_numeric_columns())
      updateCheckboxGroupInput(session, "factor_cols_selected", choices = obj$get_factor_columns())
      updateCheckboxGroupInput(session, "no_type_cols_selected", choices = obj$get_no_type_columns())
    })    

    observeEvent(input$make_categorical, {
      mutate_preprocess(function(obj) {
        obj$set_factor_columns(input$numeric_cols_selected)
      })
    })

    observeEvent(input$make_numeric, {
      mutate_preprocess(function(obj) {
        obj$set_numeric_columns(input$factor_cols_selected)
      })
    })

    observeEvent(input$make_categorical_no_type, {
      mutate_preprocess(function(obj) {
        obj$set_factor_columns(input$no_type_cols_selected)
      })
    })

    observeEvent(input$make_numeric_no_type, {
      mutate_preprocess(function(obj) {
        obj$set_numeric_columns(input$no_type_cols_selected)
      })
    })

  })
}
