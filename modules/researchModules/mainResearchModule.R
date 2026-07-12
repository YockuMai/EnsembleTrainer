practic_ui <- function() {
  tabsetPanel(
    tabPanel("Загрузка данных",
      loadDataUI("load")
    ),
    tabPanel("Предобработка",
      h3("Преобработка данных"),
      preprocessUI("preprocess")
    ),
    tabPanel("Выбор моделей и настройка параметров",
      h3("Параметры моделей"),
      modelParamsUI("model_params")
    ),
    tabPanel("Метрики точности",
      h3("Метрики точности"),
      p("Здесь будут метрики точности модели.")
    )
  )
}