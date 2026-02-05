preprocessUI <- function(id) {
  ns <- NS(id)
  navlistPanel(
    widths = c(2, 10),  # Ширина левой панели и правой
    tabPanel("Данные",
      tabsetPanel(type = "pills",
        tabPanel("Просмотр",
        #TODO: Предобработанные данные и их summary
          h4("Предобработанные данные"),
          DT::dataTableOutput(ns("data_overview")),

          h4("Статистика по признакам"),
          htmlOutput(ns("data_summary")),
          htmlOutput(ns("missing_info")),
          htmlOutput(ns("outliers_info")),
          htmlOutput(ns("scaling_info"))
        ),

        tabPanel("Смена типа признаков",
          fluidRow(
            # Левая колонка: числовые признаки
            column(6,
              div(style = "display: flex; justify-content: flex-start; align-items: center;",
                  h4("Числовые признаки"),
                  actionButton(ns("make_categorical"), "Сделать категориальными",
                               style = "margin-left: 10px;")
              ),
              checkboxGroupInput(ns("numeric_cols_selected"), label = NULL, choices = NULL)
            ),

            # Правая колонка: категориальные признаки
            column(6,
              div(style = "display: flex; justify-content: flex-start; align-items: center;",
                  h4("Категориальные признаки"),
                  actionButton(ns("make_numeric"), "Сделать числовыми",
                               style = "margin-left: 10px;")
              ),
              checkboxGroupInput(ns("factor_cols_selected"), label = NULL, choices = NULL)
            )
          ),

          # ---------------------------
          # Нижний ряд: признаки с неопределённым типом
          # ---------------------------
          uiOutput(ns("no_type_controls"))
        ),

        tabPanel("Переименование столбцов",
        #TODO: Список полей с уже забитыми столбцами 
          verbatimTextOutput(ns("data_rename"))
        ),

        tabPanel("Удаление столбцов",
        #TODO: Список столбцов с крестиками
          verbatimTextOutput(ns("data_remove"))
        )
      )
    ),
    
    tabPanel("Обработка пропусков",
             h3("Обработка пропущенных значений"),
             # Содержимое для пропусков
             
    ),
    
    tabPanel("Обработка выбросов",
            
             # Содержимое для выбросов

    ),
    
    tabPanel("Масштабирование",
             h3("Масштабирование данных"),
             # Содержимое для масштабирования

    )
  )
  
}
