preprocessUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3,
      radioButtons(ns("preprocessing_step"), "",
                   choices = c("Пропущенные значения" = "missing",
                               "Выбросы" = "outliers",
                               "Масштабирование" = "scaling",
                               "Выбор признаков" = "features"),
                   selected = "missing"),
      hr(),
      actionButton(ns("apply_preprocessing"), "Применить",
                  class = "btn-primary btn-block"),
      actionButton(ns("reset_preprocessing"), "Сбросить",
                  class = "btn-secondary btn-block")
    ),

    mainPanel(
      width = 9,
      conditionalPanel(
        condition = paste0("input['", ns("preprocessing_step"), "'] == 'missing'"),
        h3("Обработка пропущенных значений"),
        uiOutput(ns("missing_stats")),
        checkboxInput(ns("handle_missing"), "Обрабатывать пропущенные значения", value = FALSE),
        conditionalPanel(
          condition = paste0("input['", ns("handle_missing"), "'] == true"),
          selectInput(ns("missing_method"), "Метод обработки:",
                     choices = c("Среднее (заполнение)" = "mean",
                                 "Удалить строки" = "delete"),
                     selected = "mean")
        )
      ),

      conditionalPanel(
        condition = paste0("input['", ns("preprocessing_step"), "'] == 'outliers'"),
        h3("Обработка выбросов"),
        checkboxInput(ns("handle_outliers"), "Обрабатывать выбросы", value = FALSE),
        conditionalPanel(
          condition = paste0("input['", ns("handle_outliers"), "'] == true"),
          selectInput(ns("outlier_method"), "Метод обработки:",
                     choices = c("Заменить границами" = "replace",
                                 "Удалить" = "remove"),
                     selected = "replace"),
          numericInput(ns("iqr_multiplier"), "Множитель IQR:",
                      value = 1.5, min = 1, max = 3, step = 0.1)
        )
      ),

      conditionalPanel(
        condition = paste0("input['", ns("preprocessing_step"), "'] == 'scaling'"),
        h3("Масштабирование данных"),
        radioButtons(ns("scaling_method"), "Метод масштабирования:",
                   choices = c("Без масштабирования" = "none",
                               "Нормализация (0-1)" = "normalize",
                               "Стандартизация (z-score)" = "standardize"),
                   selected = "none")
      ),

      conditionalPanel(
        condition = paste0("input['", ns("preprocessing_step"), "'] == 'features'"),
        h3("Выбор признаков"),
        checkboxGroupInput(ns("columns_to_remove"), "Столбцы для удаления:",
                          choices = NULL),
        actionButton(ns("update_columns"), "Обновить список столбцов")
      ),

      conditionalPanel(
        condition = paste0("input['", ns("preprocessing_applied"), "'] == true"),
        h3("Результаты предобработки"),
        uiOutput(ns("preprocessing_status")),
        hr(),
        tabsetPanel(
          tabPanel("Исходные данные",
                   h4("Статистика исходных данных"),
                   uiOutput(ns("original_summary")),
                   h4("Предварительный просмотр"),
                   DT::dataTableOutput(ns("original_data_table"))
          ),
          tabPanel("Обработанные данные",
                   h4("Статистика обработанных данных"),
                   uiOutput(ns("processed_summary")),
                   h4("Предварительный просмотр"),
                   DT::dataTableOutput(ns("processed_data_table"))
          )
        )
      )
    )
  )
}
