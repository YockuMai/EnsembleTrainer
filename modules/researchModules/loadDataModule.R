source("R/data_load_functions.R")   # теперь используем функциональные функции

loadDataUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3,
      fileInput(ns("file"), "Выберите CSV файл",
                accept = c(".csv"),
                buttonLabel = "Обзор...",
                placeholder = "Файл не выбран"),
      selectInput(ns("sep"), "Разделитель:",
                  choices = c("Автоматически" = "auto",
                              "Точка с запятой (;)" = ";",
                              "Запятая (,)" = ",",
                              "Табуляция (\t)" = "\t",
                              "Пайп (|)" = "|"),
                  selected = "auto"),
      checkboxInput(ns("has_factor"), "Определить факторы автоматически", value = TRUE),
      actionButton(ns("load"), "Загрузить данные"),
      actionButton(ns("clear"), "Очистить данные"),
    ),
    
    mainPanel(
      width = 9,
      DT::dataTableOutput(ns("dataTable"))
    )
  )
}

loadDataServer <- function(id, session_data) {
  moduleServer(id, function(input, output, session) {
    
    # Функция для чтения данных с диска (для отображения)
    get_original_data <- reactive({
      path <- session_data$original_data_path
      if (is.null(path) || !file.exists(path)) return(NULL)
      load_data_frame(path)
    })
    
    observeEvent(input$load, {
      req(input$file)
      tryCatch({
        sep <- if (input$sep == "auto") NULL else input$sep
        res <- load_csv(filepath = input$file$datapath,
                        sep = sep,
                        stringsAsFactors = input$has_factor)
        df <- res$data
        
        # Сохраняем датафрейм на диск
        user_id <- session_data$user_id
        req(user_id)
        fst_path <- save_data_frame(df, user_id, "original_data")
        session_data$original_data_path <- fst_path
        
        # Освобождаем память
        rm(df)
        gc()
        
        # Обновляем метаданные в SQLite (через save_user_data)
        # Можно сделать через автосохранение, но для надёжности вызовем явно
        save_user_data(user_id, session_data)
        
        sep_display <- switch(res$sep,
                              ";"="точка с запятой",
                              ","="запятая",
                              "\t"="табуляция",
                              "|"="вертикальная черта",
                              res$sep)
        msg <- if (input$sep == "auto") {
          paste("Данные загружены. Разделитель:", sep_display)
        } else {
          paste("Данные загружены с разделителем:", sep_display)
        }
        showNotification(msg, type = "message")
      }, error = function(e) {
        showNotification(paste("Ошибка:", e$message), type = "error")
      })
    })
    
    observeEvent(input$clear, {
      # Удаляем файл, если он существует
      path <- session_data$original_data_path
      if (!is.null(path) && file.exists(path)) file.remove(path)
      session_data$original_data_path <- NULL
      showNotification("Исходные данные очищены", type = "message")
    })
    
    output$dataTable <- DT::renderDT({
      df <- get_original_data()
      req(df)
      datatable(df, options = list(
        pageLength = 10, scrollX = TRUE, searching = TRUE, ordering = TRUE,
        language = list(
          search = "Поиск:",
          lengthMenu = "Показать _MENU_ записей",
          info = "Показаны _START_ до _END_ из _TOTAL_ записей",
          infoEmpty = "Нет данных",
          infoFiltered = "(отфильтровано из _MAX_ записей)",
          paginate = list('first'="Первая", 'last'="Последняя",
                          'next'="Следующая", 'previous'="Предыдущая")
        )
      ), rownames = FALSE)
    })
  })
}