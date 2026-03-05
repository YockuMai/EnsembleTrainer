source("R/preprocessData.R")

preprocessServer <- function(id, session_data) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    preprocess_obj <- reactiveVal(NULL)

    #observe({
    #  if (!is.null(session_data$preprocess_obj) && is.null(preprocess_obj())) {
    #    preprocess_obj(session_data$preprocess_obj)   # восстановление
    #  } else if (!is.null(session_data$original_data) && is.null(preprocess_obj())) {
    #    preprocess_obj(PreprocessData$new(session_data$original_data))  # новый объект
    #  } else if (is.null(session_data$original_data)) {
    #    preprocess_obj(NULL)
    #  }
    #})

    #observeEvent(session_data$preprocess_obj, {
#
    #}, ignoreNULL = FALSE)

    observeEvent(session_data$original_data, {
      if (!is.null(session_data$preprocess_obj) && is.null(preprocess_obj())) {
        preprocess_obj(session_data$preprocess_obj)   # восстановление
      } else if (!is.null(session_data$original_data)) {
        preprocess_obj(PreprocessData$new(session_data$original_data))  # новый объект
      } else if (is.null(session_data$original_data)) {
        preprocess_obj(NULL)
      }
    }, ignoreNULL = FALSE)

    mutate <- function(f) {
      obj <- preprocess_obj()
      if (is.null(obj)) return(NULL)

      f(obj)                 # мутируем объект

      # Форсируем обновление reactiveVal — создаём копию объекта
      preprocess_obj(obj$clone(deep = TRUE))  # <- лучше: клонируем объект
    }

    observeEvent(preprocess_obj(), {
      session_data$preprocess_obj <- preprocess_obj()
    }, ignoreNULL = FALSE)

    # Данные -> Просмотр
    output$data_overview <- DT::renderDataTable({
      obj <- preprocess_obj()

      if (is.null(obj)) {
        return(
          datatable(
            data.frame(Error = "Данные не загружены"),
            options = list(
              searching = FALSE,
              paging = FALSE,
              info = FALSE
            ),
            rownames = FALSE
          )
        )
      }

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

    output$data_info <- renderUI({
      obj <- preprocess_obj()

      if (is.null(obj)) {
        return(NULL)
      }

      stat_missing <- obj$get_missing_statistic()
      stat_outliers <- obj$get_outliers_statistic(iqr_multiplier = input$iqr_mult)

      tagList(
        h5("Статистика по столбцам"),
        tags$pre(
          paste(capture.output(summary(obj$get_data())),
                collapse = "\n")
        ),
        br(),

        h5("Пропуски"),
        tags$pre(
          paste(
            "Всего строк: ", stat_missing$rows,
            "\nСтрок с пропусками: ", stat_missing$count,
            "\nПроцент: ", stat_missing$percentage, "%"
          )
        ),
        br(),

        h5("Выбросы"),
        if (stat_outliers$total_outliers == 0) {
          tags$pre("Выбросы не обнаружены")
        } else {
          tags$pre(
            paste(
              "Всего выбросов:", stat_outliers$total_outliers,
              "\nКолонки:",
              paste(names(stat_outliers$outliers_by_column),
                    collapse = ", ")
            )
          )
        },
        br(),

        h5("Масштабирование"),
        tags$pre(
          paste("Текущее масштабирование:",
                obj$get_scaling_type())
        )
      )
    })

    # Данные -> Смена типа признака
    observe({
      obj <- preprocess_obj()
      if (is.null(obj)) {
        updateCheckboxGroupInput(session, "numeric_cols_selected", choices = character(0), selected = character(0))
        updateCheckboxGroupInput(session, "factor_cols_selected", choices = character(0), selected = character(0))
        updateCheckboxGroupInput(session, "no_type_cols_selected", choices = character(0), selected = character(0))
      } else {
        updateCheckboxGroupInput(session, "numeric_cols_selected", choices = obj$get_numeric_columns())
        updateCheckboxGroupInput(session, "factor_cols_selected", choices = obj$get_factor_columns())
        updateCheckboxGroupInput(session, "no_type_cols_selected", choices = obj$get_no_type_columns())
      }
    })    

    output$no_type_controls <- renderUI({
      obj <- preprocess_obj()

      if (!is.null(obj)) no_type_cols <- obj$get_no_type_columns() else return(NULL)
      if (length(no_type_cols) == 0) return(NULL)

      tagList(
        fluidRow(
          column(12,
            div(style = "display: flex; align-items: center;",
                h4("Признаки с неопределённым типом"),
                div(
                  actionButton(ns("make_categorical_no_type"), "Сделать категориальными"),
                  actionButton(ns("make_numeric_no_type"), "Сделать числовыми"),
                  style = "display: flex; gap: 10px; margin-left: 10px;"
                )
            ),
            checkboxGroupInput(ns("no_type_cols_selected"), label = NULL, choices = no_type_cols)
          )
        )
      )
    })

    observeEvent(input$make_categorical, {
      withCallingHandlers({
        mutate(function(obj) {
          obj$set_factor_columns(input$numeric_cols_selected)
        })
      }, warning = function(w) {
        showNotification(conditionMessage(w), type = "error")
      })
    })

    observeEvent(input$make_numeric, {
      withCallingHandlers({
        mutate(function(obj) {
          obj$set_numeric_columns(input$factor_cols_selected)
        })
      }, warning = function(w) {
        showNotification(conditionMessage(w), type = "error")
      })
      
    })

    observeEvent(input$make_categorical_no_type, {
      withCallingHandlers({
        mutate(function(obj) {
          obj$set_factor_columns(input$no_type_cols_selected)
        })
      }, warning = function(w) {
        showNotification(conditionMessage(w), type = "error")
      })
    })

    observeEvent(input$make_numeric_no_type, {
      withCallingHandlers({
        mutate(function(obj) {
          obj$set_numeric_columns(input$no_type_cols_selected)
        })
      }, warning = function(w) {
        showNotification(conditionMessage(w), type = "error")
      })
    })

    # Данные -> Переименование столбцов
    output$data_rename <- renderUI({
      obj <- preprocess_obj()

      if (is.null(obj)) {
        return(div("Данные отсутствуют"))
      }

      cols <- colnames(obj$get_data())  # или colnames(obj$data)

      tagList(
        actionButton(ns("save_names"), "Сохранить"),
        br(), br(),
        lapply(cols, function(col) {
          fluidRow(
            column(6, textInput(ns(paste0("rename_", col)),
                                label = NULL,
                                value = col))
          )
        })
      )
    })
  
    observeEvent(input$save_names, {
      obj <- preprocess_obj()
      req(obj)

      cols <- colnames(obj$get_data())

      new_names <- sapply(cols, function(col) {
        input[[paste0("rename_", col)]]
      }, USE.NAMES = FALSE)

      rename_vector <- setNames(new_names, cols)
      tryCatch(
        {
          mutate(function(obj) {
            obj$set_columns_name(rename_vector)
          })
          showNotification("Имена столбцов обновлены", type = "message")
        },
        error = function(e) {
          showNotification(
            paste("Ошибка:", e$message),
            type = "error"
          )
        }
      )
    })

    #Данные -> Удаление столбцов
    output$data_remove <- renderUI({
      obj <- preprocess_obj()   # ваш reactiveVal с R6 объектом

      if (is.null(obj)) {
        return(div("Данные отсутствуют"))
      }

      cols <- colnames(obj$get_data())
      if (length(cols) == 0) return(div("Нет столбцов для удаления"))

      tagList(
        actionButton(ns("remove_cols"), "Удалить выбранные"),
        br(), br(),
        checkboxGroupInput(
          ns("cols_to_remove"),
          label = "Выберите столбцы для удаления",
          choices = cols
        )
      )
    })

    # Обработчик кнопки
    observeEvent(input$remove_cols, {
      obj <- preprocess_obj()
      req(obj)

      selected_cols <- input$cols_to_remove
      if (is.null(selected_cols) || length(selected_cols) == 0) {
        showNotification("Столбцы для удаления не выбраны", type = "warning")
        return()
      }

      tryCatch(
        {
          mutate(function(obj) {
            obj$remove_columns(selected_cols)  # метод R6, который удаляет столбцы из self$data
          })
          showNotification("Столбцы успешно удалены", type = "message")
        },
        error = function(e) {
          showNotification(paste("Ошибка:", e$message), type = "error")
        }
      )
    })

    # Обработка выбросов
    output$outliers <- renderUI({

      obj <- preprocess_obj()
      req(obj)

      num_cols <- obj$get_numeric_columns()

      if (length(num_cols) == 0) {
        return(div("Числовые столбцы отсутствуют"))
      }

      tagList(
      
        h4("Общая статистика выбросов"),

        verbatimTextOutput(ns("outliers_total_stat")),

        br(),

        sliderInput(
          ns("iqr_mult"),
          "Множитель IQR",
          min = 0.5,
          max = 3,
          value = 1.5,
          step = 0.1
        ),

        radioButtons(
          ns("outlier_method"),
          "Метод обработки",
          choices = c(
            "Без изменений" = "none",
            "Заменить граничными значениями" = "replace",
            "Удалить строки" = "delete"
          )
        ),

        actionButton(ns("apply_outliers"), "Применить"),

        hr(),

        lapply(num_cols, function(col) {
        
          plotname  <- paste0("plot_", col)
          statsname <- paste0("stats_", col)

          output[[plotname]] <- renderPlot({
          
            iqr_mult <- input$iqr_mult
            req(iqr_mult)

            data <- preprocess_obj()$get_data()
            x <- data[[col]]

            q1 <- quantile(x, 0.25, na.rm = TRUE)
            q3 <- quantile(x, 0.75, na.rm = TRUE)
            iqr <- q3 - q1

            lower <- q1 - iqr_mult * iqr
            upper <- q3 + iqr_mult * iqr

            boxplot(x, main = col, horizontal = TRUE)
            abline(v = lower, lty = 2)
            abline(v = upper, lty = 2)
          })

          output[[statsname]] <- renderText({
          
            stats <- preprocess_obj()$
              get_outliers_statistic(iqr_multiplier = input$iqr_mult)

            col_stat <- stats$outliers_by_column[[col]]

            if (is.null(col_stat)) {
              return("Выбросы отсутствуют")
            }

            paste0(
              "Количество: ", col_stat$count,
              "\nПроцент: ", col_stat$percentage, "%",
              "\nНижняя граница: ", round(col_stat$lower, 4),
              "\nВерхняя граница: ", round(col_stat$upper, 4)
            )
          })

          fluidRow(
            column(
              12,
              strong(col),
              plotOutput(ns(plotname), height = "250px"),
              verbatimTextOutput(ns(statsname)),
              hr()
            )
          )
        })
      )
    })

    output$outliers_total_stat <- renderText({

      obj <- preprocess_obj()
      req(obj)

      stats <- obj$get_outliers_statistic(
        iqr_multiplier = input$iqr_mult
      )

      paste0(
        "Всего выбросов: ", stats$total_outliers,
        "\nСтолбцы с выбросами: ",
        paste(names(stats$outliers_by_column), collapse = ", ")
      )
    })

    observeEvent(input$apply_outliers, {

      method <- input$outlier_method
      if (method == "none") return()

      obj <- preprocess_obj()

      tryCatch({
      
        obj$clear_outliers(
          method = method,
          iqr_multiplier = input$iqr_mult
        )

        preprocess_obj(obj$clone(deep = TRUE))

        showNotification("Обработка выбросов выполнена",
                         type = "message")

      }, error = function(e) {
      
        showNotification(
          paste("Ошибка:", e$message),
          type = "error"
        )

      })

    })


  })
}
