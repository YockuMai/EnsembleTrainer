source("ui/practic/loadDataUI.R")
source("ui/practic/preprocessUI.R")

practicUI <- function() {
  tabsetPanel(
    tabPanel("Загрузка данных",
             loadDataUI("load")
    ),
    tabPanel("Предобработка",
             preprocessUI("preprocess")
    ),
    tabPanel("Параметры моделей",
             h3("Параметры моделей"),
             p("Здесь будет функционал заадния параметров.")
    ),
    tabPanel("Обучение",
             h3("Обучение модели"),
             p("Здесь будет функционал обучения.")
    ),
    tabPanel("Прогнозирование",
             h3("Прогнозирование"),
             p("Здесь будет прогнозирование на тестовой части.")
    )
  )
}
