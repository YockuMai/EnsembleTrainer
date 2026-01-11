source("R/dataLoader.R")

dataLoader <- DataLoader$new()

loadDataServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    data_reactive <- reactiveValues(data = NULL, file = NULL)
    user_id_rv <- reactiveVal(NULL)

    # Получаем или создаем постоянный user_id через cookie
    observe({
      if (is.null(user_id_rv())) {
        user_id <- cookies::get_cookie("user_id", session = session)
        if (is.null(user_id) || user_id == "") {
          user_id <- paste0("user_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(9999,1))
          cookies::set_cookie("user_id", user_id, session = session)
        }
        user_id_rv(user_id)
      }
    })

    session_dir <- reactive({
      req(user_id_rv())
      file.path("data", user_id_rv())
    })

    # Автоматическая загрузка при запуске
    observe({
      req(user_id_rv())
      csv_file <- file.path(session_dir(), "last_uploaded.csv")
      sep_file <- file.path(session_dir(), "sep.txt")
      if (file.exists(csv_file)) {
        sep <- if (file.exists(sep_file)) readLines(sep_file, n=1) else ";"
        result <- dataLoader$csvLoad(csv_file, sep = sep)
        if (!is.null(result) && result) {
          data_reactive$data <- dataLoader$getData()
          data_reactive$file <- csv_file
        }
      }
    })

    observeEvent(input$load, {
      req(input$file)
      req(user_id_rv())

      # Загружаем данные
      result <- dataLoader$csvLoad(input$file$datapath, input$sep)

      if (!is.null(result) && result) {
        # Успешная загрузка
        data_reactive$data <- dataLoader$getData()
        # Сохраняем файл для persistence
        dir.create(session_dir(), showWarnings = FALSE, recursive = TRUE)
        csv_path <- file.path(session_dir(), "last_uploaded.csv")
        sep_path <- file.path(session_dir(), "sep.txt")
        file.copy(input$file$datapath, csv_path, overwrite = TRUE)
        writeLines(as.character(input$sep), sep_path)
        data_reactive$file <- csv_path
        output$status <- renderUI({
          div(style = "color: green;", "Данные успешно загружены!")
        })
      } else {
        # Ошибка
        data_reactive$data <- NULL
        data_reactive$file <- NULL
        error_msg <- dataLoader$getError()
        output$status <- renderUI({
          div(style = "color: red;", paste("Ошибка:", error_msg))
        })
      }
    })

    observeEvent(input$clear, {
      # Удаляем файлы
      if (!is.null(data_reactive$file) && file.exists(data_reactive$file)) {
        file.remove(data_reactive$file)
      }
      sep_file <- file.path(session_dir(), "sep.txt")
      if (file.exists(sep_file)) {
        file.remove(sep_file)
      }
      data_reactive$data <- NULL
      data_reactive$file <- NULL
      output$status <- renderUI({
        div(style = "color: blue;", "Исходные данные очищены.")
      })
    })

    output$dataTable <- DT::renderDT({
      data_reactive$data
    })
  })
}
