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
             uiOutput(ns("missing_values"))
    ),
    
    tabPanel("Обработка выбросов",
             # Содержимое для выбросов
             uiOutput(ns("outliers"))
    )
  )
  
}

source("R/preprocess_functions.R")
source("R/db_functions.R")   # для save_data_frame, load_data_frame, save_user_data

preprocessServer <- function(id, session_data) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ---- Реактивное значение для хранения текущего датафрейма (в памяти) ----
    current_data <- reactiveVal(NULL)
    
    # ---- Наблюдатель за изменением путей ----
    # Срабатывает при изменении original_data_path или preprocess_path,
    # загружает данные с диска и помещает в current_data
    observe({
      # Сначала пытаемся загрузить предобработанные
      path <- NULL
      if (!is.null(session_data$preprocess_path) && file.exists(session_data$preprocess_path)) {
        path <- session_data$preprocess_path
      } else if (!is.null(session_data$original_data_path) && file.exists(session_data$original_data_path)) {
        path <- session_data$original_data_path
      }
      
      if (!is.null(path)) {
        tryCatch({
          df <- load_data_frame(path)
          current_data(df)
        }, error = function(e) {
          showNotification(paste("Ошибка загрузки данных:", e$message), type = "error")
          current_data(NULL)
        })
      } else {
        # Нет данных – сбрасываем
        current_data(NULL)
      }
    }) %>% bindEvent(
      session_data$original_data_path,
      session_data$preprocess_path,
      ignoreNULL = FALSE
    )
    
    # ---- Сохранение результата предобработки на диск ----
    save_current_data <- function(df) {
      user_id <- session_data$user_id
      if (is.null(user_id)) {
        showNotification("Ошибка: пользователь не идентифицирован", type = "error")
        return(NULL)
      }
      # Сохраняем датафрейм в FST
      fst_path <- save_data_frame(df, user_id, "preprocess")
      # Обновляем путь в session_data
      session_data$preprocess_path <- fst_path
      # Обновляем метаданные в SQLite
      save_user_data(user_id, session_data)
      # Возвращаем путь
      fst_path
    }
    
    # ---- Вспомогательная функция для всех преобразований ----
    apply_transform <- function(transform_func, ..., success_msg = "Операция выполнена") {
      data <- current_data()
      if (is.null(data)) {
        showNotification("Нет данных для обработки", type = "warning")
        return()
      }
      tryCatch({
        new_data <- transform_func(data, ...)
        # Проверяем, что результат не пустой
        if (is.null(new_data) || nrow(new_data) == 0) {
          showNotification("Результат обработки пуст", type = "warning")
          return()
        }
        # Сохраняем на диск
        save_current_data(new_data)
        # Обновляем current_data (чтобы UI обновился)
        current_data(new_data)
        showNotification(success_msg, type = "message")
        # Освобождаем память
        rm(new_data, data); gc()
      }, error = function(e) {
        showNotification(paste("Ошибка:", e$message), type = "error")
      })
    }
    
    # ---- Очистка предобработанных данных при удалении оригинальных ----
    observeEvent(session_data$original_data_path, {
      if (is.null(session_data$original_data_path)) {
        # Удаляем файл предобработки, если он есть
        prep_path <- session_data$preprocess_path
        if (!is.null(prep_path) && file.exists(prep_path)) {
          file.remove(prep_path)
        }
        session_data$preprocess_path <- NULL
        # Обновляем БД
        user_id <- session_data$user_id
        if (!is.null(user_id)) {
          save_user_data(user_id, session_data)
        }
        # current_data обновится через observe
        showNotification("Предобработанные данные очищены", type = "message")
      }
    }, ignoreNULL = FALSE)
    
    # ---- ПРОСМОТР ДАННЫХ (таблица) ----
    output$data_overview <- DT::renderDataTable({
      data <- current_data()
      if (is.null(data)) {
        return(
          datatable(
            data.frame(Error = "Данные не загружены"),
            options = list(searching = FALSE, paging = FALSE, info = FALSE),
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
    
    # ---- ИНФОРМАЦИЯ О ДАННЫХ ----
    output$data_info <- renderUI({
      data <- current_data()
      if (is.null(data)) return(NULL)
      
      stat_missing <- get_missing_statistic(data)
      stat_outliers <- get_outliers_statistic(data, iqr_multiplier = input$iqr_mult %||% 1.5)
      
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
            "\nКолонки:", paste(names(stat_outliers$outliers_by_column), collapse = ", ")
          ))
        }
      )
    })
    
    # ---- СМЕНА ТИПА ПРИЗНАКОВ ----
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
    
    # Преобразования
    observeEvent(input$make_categorical, {
      req(current_data(), input$numeric_cols_selected)
      apply_transform(set_factor_columns, columns = input$numeric_cols_selected,
                      success_msg = "Числовые колонки преобразованы в факторы")
    })
    
    observeEvent(input$make_numeric, {
      req(current_data(), input$factor_cols_selected)
      apply_transform(set_numeric_columns, columns = input$factor_cols_selected,
                      success_msg = "Факторы преобразованы в числовые")
    })
    
    observeEvent(input$make_categorical_no_type, {
      req(current_data(), input$no_type_cols_selected)
      apply_transform(set_factor_columns, columns = input$no_type_cols_selected,
                      success_msg = "Колонки преобразованы в факторы")
    })
    
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
      apply_transform(clear_outliers, method = method, iqr_multiplier = input$iqr_mult,
                      success_msg = "Обработка выбросов выполнена")
    })
    
    # ---- ОБРАБОТКА ПРОПУСКОВ ----
    output$missing_values <- renderUI({
      data <- current_data()
      req(data)
      all_cols <- colnames(data)
      
      tagList(
        h4("Общая статистика пропусков"),
        verbatimTextOutput(ns("missing_total_stat")),
        br(),
        radioButtons(ns("missing_method"), "Метод обработки",
                     choices = c(
                       "Удалить строки с пропусками" = "delete",
                       "Заполнить средним/модой" = "mean"
                     )),
        actionButton(ns("apply_missing"), "Применить"),
        hr(),
        
        lapply(all_cols, function(col) {
          statsname <- paste0("missing_stats_", col)
          
          output[[statsname]] <- renderText({
            req(current_data())
            stats <- get_missing_statistic(current_data())
            col_stat <- stats$missing_by_column[[col]]
            if (is.null(col_stat)) return("Пропуски отсутствуют")
            paste0(
              "Количество: ", col_stat$count,
              "\nПроцент: ", col_stat$percentage, "%"
            )
          })
          
          fluidRow(
            column(12,
                   strong(col),
                   verbatimTextOutput(ns(statsname)),
                   hr()
            )
          )
        })
      )
    })
    
    output$missing_total_stat <- renderText({
      data <- current_data()
      req(data)
      stats <- get_missing_statistic(data)
      paste0(
        "Всего строк: ", stats$rows,
        "\nСтрок с пропусками: ", stats$count,
        "\nПроцент: ", stats$percentage, "%"
      )
    })
    
    observeEvent(input$apply_missing, {
      method <- input$missing_method
      apply_transform(clear_missing, method = method,
                      success_msg = "Обработка пропусков выполнена")
    })
    
  })
}