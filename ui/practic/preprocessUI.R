preprocessUI <- function(id) {
  ns <- NS(id)
  navlistPanel(
    widths = c(2, 10),  # Ширина левой панели и правой
    tabPanel("Просмотр данных",
      tabsetPanel(type = "pills",
      
        tabPanel("Просмотр",
        #TODO: Предобработанные данные и их summary
          h4("Предобработанные данные"),
          DT::dataTableOutput(ns("data_overview")),

          h4("Статистика по признакам"),
          htmlOutput(ns("data_summary"))
        ),

        tabPanel("Смена типа признаков",
          h4("Числовые признаки"),
          actionButton(ns("make_categorical"), "Сделать категориальными"),
          checkboxGroupInput(ns("numeric_cols_selected"), "Выберите столбцы:", choices = NULL),

          h4("Категориальные признаки"),
          actionButton(ns("make_numeric"), "Сделать числовыми"),
          checkboxGroupInput(ns("factor_cols_selected"), "Выберите столбцы:", choices = NULL),

          h4("Признаки с неопределённым типом"),
          actionButton(ns("make_categorical_no_type"), "Сделать категориальными"),
          actionButton(ns("make_numeric_no_type"), "Сделать числовыми"),
          checkboxGroupInput(ns("no_type_cols_selected"), "Выберите столбцы:", choices = NULL)
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
             textOutput(ns("missing_info")),
             selectInput(ns("missing_method"), "Метод:",
                        choices = c("Замена средним/модой" = "mean",
                                   "Удаление строк" = "delete")),
             actionButton(ns("clear_missing"), "Применить")
    ),
    
    tabPanel("Обработка выбросов",
            fluidRow(
              column(8, h3("Обработка выбросов")),
              column(4, div(style = "", 
                              actionButton(ns("clear_outliers"), "Применить")))
            ),
             # Содержимое для выбросов
            textOutput(ns("outliers_info")),
            selectInput(ns("outlier_method"), "Метод:",
                        choices = c("Замена границами" = "replace",
                                   "Удаление строк" = "delete"))
    ),
    
    tabPanel("Масштабирование",
             h3("Масштабирование данных"),
             # Содержимое для масштабирования
             textOutput(ns("scaling_info")),
             selectInput(ns("scaling_method"), "Метод:",
                        choices = c("Нормализация (0-1)" = "normalize",
                                   "Стандартизация (z-score)" = "standardize",
                                   "Отмена масштабирования" = "denormalize")),
             actionButton(ns("apply_scaling"), "Применить")
    )
  )
  
}
