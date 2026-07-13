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
    tabPanel("Обучение моделей",
      h3("Обучение моделей"),
      modelTrainingUI("model_training")
    ),
    tabPanel("Прогнозирование",
      h3("Прогнозирование"),
      #modelPredictionUI("model_prediction")
    )
  )
}