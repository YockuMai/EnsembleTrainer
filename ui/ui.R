source("ui/practicUI.R")
source("ui/lectionUI.R")
source("ui/testUI.R")

create_ui <- function() {
  ui <- fluidPage(
    cookies::cookie_dependency(),
    titlePanel("Тренажёр по ансамблевым методам классификации"),
  
    tabsetPanel(type = "pills",
      tabPanel("Теория",
               lectionUI()
      ),
      tabPanel("Практика",
               practicUI()
      ),
      tabPanel("Тестирование",
               testUI()
      )
    )
  )
}