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
    
    observeEvent(input$load, {
      req(input$file)
      
      tryCatch({
        
        # Определяем разделитель: если "auto" – передаём NULL, иначе выбранный
        sep <- if (input$sep == "auto") NULL else input$sep
        
        # Загружаем данные с помощью чистой функции
        res <- load_csv(
          filepath = input$file$datapath,
          sep = sep,
          stringsAsFactors = input$has_factor
        )
        
        session_data$original_data <- res$data
        
        # Человеческое имя разделителя (используем res$sep – определённый автоматически или указанный)
        sep_display <- switch(
          res$sep,
          ";" = "точка с запятой",
          "," = "запятая",
          "\t" = "табуляция",
          "|" = "вертикальная черта",
          res$sep
        )
        
        msg <- if (input$sep == "auto") {
          paste("Данные успешно загружены. Автоматически определён разделитель:", sep_display)
        } else {
          paste("Данные успешно загружены с разделителем:", sep_display)
        }
        
        showNotification(msg, type = "message")
        
      }, error = function(e) {
        showNotification(paste("Ошибка:", e$message), type = "error")
      })
    })
    
    observeEvent(input$clear, {
      session_data$original_data <- NULL
      showNotification("Исходные данные, предобработка и модели очищены.", type = "message")
    })
    
    output$dataTable <- DT::renderDT({
      req(session_data$original_data)
      datatable(
        session_data$original_data,
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
  })
}