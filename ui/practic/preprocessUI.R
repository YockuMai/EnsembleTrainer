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

          htmlOutput(ns("data_info"))
        ),

        tabPanel("Смена типа признаков",
          fluidRow(
            column(6,
              div(style = "display: flex; justify-content: flex-start; align-items: center;",
                  h4("Числовые признаки"),
                  actionButton(ns("make_categorical"), "Сделать категориальными",
                               style = "margin-left: 10px;")
              ),
              checkboxGroupInput(ns("numeric_cols_selected"), label = NULL, choices = NULL)
            ),

            column(6,
              div(style = "display: flex; justify-content: flex-start; align-items: center;",
                  h4("Категориальные признаки"),
                  actionButton(ns("make_numeric"), "Сделать числовыми",
                               style = "margin-left: 10px;")
              ),
              checkboxGroupInput(ns("factor_cols_selected"), label = NULL, choices = NULL)
            )
          ),

          uiOutput(ns("no_type_controls"))
        ),

        tabPanel("Переименование столбцов",
          uiOutput(ns("data_rename"))
        ),

        tabPanel("Удаление столбцов",
          uiOutput(ns("data_remove"))
        )
      )
    ),
    
    tabPanel("Обработка пропусков",
      h3("Обработка пропущенных значений"),
      # Содержимое для пропусков
      
    ),
    
    tabPanel("Обработка выбросов",
      # Содержимое для выбросов
      uiOutput(ns("outliers"))
    ),
    
    tabPanel("Масштабирование",
             h3("Масштабирование данных"),
             # Содержимое для масштабирования

    )
  )
  
}
