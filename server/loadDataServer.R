source("R/dataLoader.R")

loadDataServer <- function(id, session_data) {
  moduleServer(id, function(input, output, session) {
    dataLoader <- DataLoader$new()

    observeEvent(input$load, {
      req(input$file)

      # Определяем разделитель автоматически
      auto_sep <- dataLoader$guessSeparator(input$file$datapath)

      # Используем автоматически определенный разделитель, если пользователь не указал явно
      sep_to_use <- if (input$sep == "auto") auto_sep else input$sep

      # Загружаем данные
      result <- dataLoader$csvLoad(input$file$datapath, sep_to_use)

      if (!is.null(result) && result) {
        # Успешная загрузка
        current_data <- session_data()
        current_data$original_data <- dataLoader$getData()
        session_data(current_data)

        # Показываем статус с информацией об использованном разделителе
        sep_display <- switch(sep_to_use,
                              ";" = "точка с запятой",
                              "," = "запятая",
                              "\t" = "табуляция",
                              "|" = "вертикальная черта",
                              " " = "пробел",
                              sep_to_use)

        status_msg <- if (input$sep == "auto") {
          paste("Данные успешно загружены! Автоматически определен разделитель:", sep_display)
        } else {
          paste("Данные успешно загружены с разделителем:", sep_display)
        }

        output$status <- renderUI({
          div(style = "color: green;", status_msg)
        })
      } else {
        # Ошибка
        error_msg <- dataLoader$getError()
        output$status <- renderUI({
          div(style = "color: red;", paste("Ошибка:", error_msg))
        })
      }
    })

    observeEvent(input$clear, {
      current_data <- session_data()
      current_data$original_data <- NULL
      current_data$processed_data <- NULL
      session_data(current_data)
      output$status <- renderUI({
        div(style = "color: blue;", "Исходные данные очищены.")
      })
    })

    output$dataTable <- DT::renderDT({
      session_data()$original_data
    })
  })
}
