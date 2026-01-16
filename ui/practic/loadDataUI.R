loadDataUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
  tagList(
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
    actionButton(ns("load"), "Загрузить данные"),
    actionButton(ns("clear"), "Очистить данные"),
    br(),
    br(),
    uiOutput(ns("status"))
          )
    ),

    mainPanel(
      DT::dataTableOutput(ns("dataTable"))
    )
  )
}
