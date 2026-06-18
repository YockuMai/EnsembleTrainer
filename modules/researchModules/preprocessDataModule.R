source("R/preprocess_functions.R")

preprocessUI <- function(id) {
  ns <- NS(id)
  navlistPanel(
    widths = c(2, 10),  # Ширина левой панели и правой
    tabPanel("Данные",
             tabsetPanel(type = "pills",
                         tabPanel("Просмотр",
                                  #TODO: Предобработанные данные и их summary
                                  h4("Предобработанные данные"),
                                  DT::dataTableOutput(ns("data_overview")),
                                  
                                  htmlOutput(ns("data_info"))
                         ),
                         
                         tabPanel("Смена типа признаков",
                                  fluidRow(
                                    column(6,
                                           div(style = "display: flex; justify-content: flex-start; align-items: center;",
                                               h4("Числовые признаки"),
                                               actionButton(ns("make_categorical"), "Сделать категориальными",
                                                            style = "margin-left: 10px;")
                                           ),
                                           checkboxGroupInput(ns("numeric_cols_selected"), label = NULL, choices = NULL)
                                    ),
                                    
                                    column(6,
                                           div(style = "display: flex; justify-content: flex-start; align-items: center;",
                                               h4("Категориальные признаки"),
                                               actionButton(ns("make_numeric"), "Сделать числовыми",
                                                            style = "margin-left: 10px;")
                                           ),
                                           checkboxGroupInput(ns("factor_cols_selected"), label = NULL, choices = NULL)
                                    )
                                  ),
                                  
                                  uiOutput(ns("no_type_controls"))
                         ),
                         
                         tabPanel("Переименование столбцов",
                                  uiOutput(ns("data_rename"))
                         ),
                         
                         tabPanel("Удаление столбцов",
                                  uiOutput(ns("data_remove"))
                         )
             )
    ),
    
    tabPanel("Обработка пропусков",
             h3("Обработка пропущенных значений"),
             # Содержимое для пропусков
             
    ),
    
    tabPanel("Обработка выбросов",
             # Содержимое для выбросов
             uiOutput(ns("outliers"))
    ),
    
    tabPanel("Масштабирование",
             h3("Масштабирование данных"),
             # Содержимое для масштабирования
             
    )
  )
  
}

preprocessServer <- function(id, session_data) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    current_data <- reactiveVal(NULL)
    scaling_params <- reactiveVal(NULL)
    
    # preprocessServer.R – исправленный блок инициализации
    observe({
      if (is.null(session_data$original_data)) {
        # Нет исходных данных – сбрасываем всё
        current_data(NULL)
        scaling_params(NULL)
      } else {
        # Исходные данные есть
        if (!is.null(session_data$preprocess_obj)) {
          # Восстанавливаем предобработанные данные
          current_data(session_data$preprocess_obj)
          if (!is.null(session_data$scaling_params)) {
            scaling_params(session_data$scaling_params)
          } else {
            scaling_params(NULL)
          }
        } else {
          # Берём оригинальные данные
          current_data(session_data$original_data)
          scaling_params(NULL)
        }
      }
    }) %>% bindEvent(
      session_data$original_data,
      session_data$preprocess_obj,
      session_data$scaling_params,
      ignoreNULL = FALSE
    )
    
    # Сохраняем текущие данные обратно в session_data
    observeEvent(current_data(), {
      session_data$preprocess_obj <- current_data()
    }, ignoreNULL = FALSE)
    
    # Сохраняем параметры масштабирования
    observeEvent(scaling_params(), {
      session_data$scaling_params <- scaling_params()
    }, ignoreNULL = FALSE)
    
    # ---- ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ДЛЯ ОБНОВЛЕНИЯ UI ----
    
    # Функция безопасного обновления current_data с уведомлениями
    apply_transform <- function(transform_func, ..., success_msg = "Операция выполнена") {
      req(current_data())
      tryCatch({
        new_data <- transform_func(current_data(), ...)
        current_data(new_data)
        showNotification(success_msg, type = "message")
      }, error = function(e) {
        showNotification(paste("Ошибка:", e$message), type = "error")
      })
    }
    
    # ---- ПРОСМОТР ДАННЫХ (таблица) ----
    output$data_overview <- DT::renderDataTable({
      data <- current_data()
      if (is.null(data)) {
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
        data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          searching = TRUE,
          ordering = TRUE,
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
    
    # ---- ИНФОРМАЦИЯ О ДАННЫХ (пропуски, выбросы, масштабирование) ----
    output$data_info <- renderUI({
      data <- current_data()
      if (is.null(data)) return(NULL)
      
      stat_missing <- get_missing_statistic(data)
      stat_outliers <- get_outliers_statistic(data, iqr_multiplier = input$iqr_mult)
      # Определяем, было ли масштабирование (по сохранённым параметрам)
      scaling_type <- if (is.null(scaling_params())) "none" else scaling_params()$type
      
      tagList(
        h5("Статистика по столбцам"),
        tags$pre(paste(capture.output(summary(data)), collapse = "\n")),
        br(),
        h5("Пропуски"),
        tags$pre(paste(
          "Всего строк: ", stat_missing$rows,
          "\nСтрок с пропусками: ", stat_missing$count,
          "\nПроцент: ", stat_missing$percentage, "%"
        )),
        br(),
        h5("Выбросы"),
        if (stat_outliers$total_outliers == 0) {
          tags$pre("Выбросы не обнаружены")
        } else {
          tags$pre(paste(
            "Всего выбросов:", stat_outliers$total_outliers,
            "\nКолонки:",
            paste(names(stat_outliers$outliers_by_column), collapse = ", ")
          ))
        },
        br(),
        h5("Масштабирование"),
        tags$pre(paste("Текущее масштабирование:", scaling_type))
      )
    })
    
    # ---- СМЕНА ТИПА ПРИЗНАКОВ ----
    # Обновление списков в чекбоксах при изменении данных
    observe({
      data <- current_data()
      if (is.null(data)) {
        updateCheckboxGroupInput(session, "numeric_cols_selected", choices = character(0))
        updateCheckboxGroupInput(session, "factor_cols_selected", choices = character(0))
        updateCheckboxGroupInput(session, "no_type_cols_selected", choices = character(0))
      } else {
        col_types <- detect_columns(data)
        updateCheckboxGroupInput(session, "numeric_cols_selected", choices = col_types$numeric)
        updateCheckboxGroupInput(session, "factor_cols_selected", choices = col_types$factor)
        updateCheckboxGroupInput(session, "no_type_cols_selected", choices = col_types$other)
      }
    })
    
    # Отображение блока "неопределённые типы"
    output$no_type_controls <- renderUI({
      data <- current_data()
      if (is.null(data)) return(NULL)
      col_types <- detect_columns(data)
      no_type_cols <- col_types$other
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
    
    # Преобразовать числовые в факторы
    observeEvent(input$make_categorical, {
      req(current_data(), input$numeric_cols_selected)
      apply_transform(set_factor_columns, columns = input$numeric_cols_selected,
                      success_msg = "Числовые колонки преобразованы в факторы")
    })
    
    # Преобразовать факторы в числовые
    observeEvent(input$make_numeric, {
      req(current_data(), input$factor_cols_selected)
      apply_transform(set_numeric_columns, columns = input$factor_cols_selected,
                      success_msg = "Факторы преобразованы в числовые")
    })
    
    # Преобразовать неопределённые в факторы
    observeEvent(input$make_categorical_no_type, {
      req(current_data(), input$no_type_cols_selected)
      apply_transform(set_factor_columns, columns = input$no_type_cols_selected,
                      success_msg = "Колонки преобразованы в факторы")
    })
    
    # Преобразовать неопределённые в числовые
    observeEvent(input$make_numeric_no_type, {
      req(current_data(), input$no_type_cols_selected)
      apply_transform(set_numeric_columns, columns = input$no_type_cols_selected,
                      success_msg = "Колонки преобразованы в числовые")
    })
    
    # ---- ПЕРЕИМЕНОВАНИЕ СТОЛБЦОВ ----
    output$data_rename <- renderUI({
      data <- current_data()
      if (is.null(data)) return(div("Данные отсутствуют"))
      cols <- colnames(data)
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
      data <- current_data()
      req(data)
      cols <- colnames(data)
      new_names <- sapply(cols, function(col) {
        input[[paste0("rename_", col)]]
      }, USE.NAMES = FALSE)
      rename_vector <- setNames(new_names, cols)
      apply_transform(set_columns_name, rename_vector = rename_vector,
                      success_msg = "Имена столбцов обновлены")
    })
    
    # ---- УДАЛЕНИЕ СТОЛБЦОВ ----
    output$data_remove <- renderUI({
      data <- current_data()
      if (is.null(data)) return(div("Данные отсутствуют"))
      cols <- colnames(data)
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
    
    observeEvent(input$remove_cols, {
      selected <- input$cols_to_remove
      if (is.null(selected) || length(selected) == 0) {
        showNotification("Столбцы для удаления не выбраны", type = "warning")
        return()
      }
      apply_transform(remove_columns, columns_to_remove = selected,
                      success_msg = "Столбцы удалены")
    })
    
    # ---- ОБРАБОТКА ВЫБРОСОВ ----
    output$outliers <- renderUI({
      data <- current_data()
      req(data)
      col_types <- detect_columns(data)
      num_cols <- col_types$numeric
      if (length(num_cols) == 0) {
        return(div("Числовые столбцы отсутствуют"))
      }
      
      tagList(
        h4("Общая статистика выбросов"),
        verbatimTextOutput(ns("outliers_total_stat")),
        br(),
        sliderInput(ns("iqr_mult"), "Множитель IQR", min = 0.5, max = 3, value = 1.5, step = 0.1),
        radioButtons(ns("outlier_method"), "Метод обработки",
                     choices = c(
                       "Без изменений" = "none",
                       "Заменить граничными значениями" = "replace",
                       "Удалить строки" = "delete"
                     )),
        actionButton(ns("apply_outliers"), "Применить"),
        hr(),
        
        lapply(num_cols, function(col) {
          plotname <- paste0("plot_", col)
          statsname <- paste0("stats_", col)
          
          output[[plotname]] <- renderPlot({
            req(current_data(), input$iqr_mult)
            x <- current_data()[[col]]
            q1 <- quantile(x, 0.25, na.rm = TRUE)
            q3 <- quantile(x, 0.75, na.rm = TRUE)
            iqr <- q3 - q1
            lower <- q1 - input$iqr_mult * iqr
            upper <- q3 + input$iqr_mult * iqr
            boxplot(x, main = col, horizontal = TRUE)
            abline(v = lower, lty = 2)
            abline(v = upper, lty = 2)
          })
          
          output[[statsname]] <- renderText({
            req(current_data(), input$iqr_mult)
            stats <- get_outliers_statistic(current_data(), iqr_multiplier = input$iqr_mult)
            col_stat <- stats$outliers_by_column[[col]]
            if (is.null(col_stat)) return("Выбросы отсутствуют")
            paste0(
              "Количество: ", col_stat$count,
              "\nПроцент: ", col_stat$percentage, "%",
              "\nНижняя граница: ", round(col_stat$lower, 4),
              "\nВерхняя граница: ", round(col_stat$upper, 4)
            )
          })
          
          fluidRow(
            column(12,
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
      data <- current_data()
      req(data, input$iqr_mult)
      stats <- get_outliers_statistic(data, iqr_multiplier = input$iqr_mult)
      paste0(
        "Всего выбросов: ", stats$total_outliers,
        "\nСтолбцы с выбросами: ",
        paste(names(stats$outliers_by_column), collapse = ", ")
      )
    })
    
    observeEvent(input$apply_outliers, {
      method <- input$outlier_method
      if (method == "none") return()
      apply_transform(clear_outliers, method = method, iqr_multiplier = input$iqr_mult,
                      success_msg = "Обработка выбросов выполнена")
    })
    
    # ---- НОРМАЛИЗАЦИЯ / СТАНДАРТИЗАЦИЯ (опционально) ----
    # Добавим две кнопки в UI (вы можете их разместить отдельно)
    # Пример:
    output$scaling_controls <- renderUI({
      tagList(
        actionButton(ns("normalize_btn"), "Нормализовать (min-max)"),
        actionButton(ns("standardize_btn"), "Стандартизировать (z-score)"),
        actionButton(ns("descale_btn"), "Отменить масштабирование")
      )
    })
    
    observeEvent(input$normalize_btn, {
      req(current_data())
      tryCatch({
        res <- normalize(current_data())
        current_data(res$data)
        scaling_params(res$params)
        showNotification("Данные нормализованы", type = "message")
      }, error = function(e) {
        showNotification(paste("Ошибка нормализации:", e$message), type = "error")
      })
    })
    
    observeEvent(input$standardize_btn, {
      req(current_data())
      tryCatch({
        res <- standardize(current_data())
        current_data(res$data)
        scaling_params(res$params)
        showNotification("Данные стандартизированы", type = "message")
      }, error = function(e) {
        showNotification(paste("Ошибка стандартизации:", e$message), type = "error")
      })
    })
    
    observeEvent(input$descale_btn, {
      req(current_data(), scaling_params())
      tryCatch({
        new_data <- denormalize_or_destandardize(current_data(), scaling_params())
        current_data(new_data)
        scaling_params(NULL)
        showNotification("Масштабирование отменено", type = "message")
      }, error = function(e) {
        showNotification(paste("Ошибка:", e$message), type = "error")
      })
    })
    
  })
}