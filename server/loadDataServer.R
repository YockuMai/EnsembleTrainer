source("R/dataLoader.R")

loadDataServer <- function(id, session_data) {
  moduleServer(id, function(input, output, session) {
    dataLoader <- DataLoader$new()

    observeEvent(input$load, {
      req(input$file)

        # Загружаем данные
        if (input$sep == "auto")
          result <- dataLoader$csv_load(input$file$datapath, NULL)
        else
          result <- dataLoader$csv_load(input$file$datapath, input$sep)

        if (result) {
          current_data <- session_data()
          current_data$original_data <- dataLoader$get_data()
          session_data(current_data)

          # Показываем статус с информацией об использованном разделителе
          sep_display <- switch(dataLoader$get_sep(),
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
        }
        else {
          output$status <- renderUI({
            div(style = "color: red;", paste("Ошибка:", dataLoader$get_error()))
          })
        }
  })
  
    observeEvent(input$clear, {
      session_data(list())
      output$status <- renderUI({
        div(style = "color: blue;", "Исходные и предобработанные данные, а также обученные модели очищены.")
      })
    })

    output$dataTable <- DT::renderDT({
      session_data()$original_data
    })
  })
}
