source("R/dataLoader.R")

loadDataServer <- function(id, session_data) {
  moduleServer(id, function(input, output, session) {

    loader <- DataLoader$new()

    observeEvent(input$load, {
      req(input$file)

      tryCatch({

        # Загружаем данные
        res <- loader$csv_load(
          filepath = input$file$datapath,
          sep = if (input$sep == "auto") NULL else input$sep,
          stringsAsFactors = input$has_factor
        )

        session_data$original_data <- res$data

        # Человеческое имя разделителя
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
  })
}
