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
