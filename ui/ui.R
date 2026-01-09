library(shiny)
source("ui/practicUI.R")
source("ui/lectionUI.R")
source("ui/testUI.R")

ui <- fluidPage(
  cookies::cookie_dependency(),
  titlePanel("Тренажёр по ансамблевым методам классификации"),

  tabsetPanel(
    tabPanel("Лекции",
             lectionUI()
    ),
    tabPanel("Практики",
             practicUI()
    ),
    tabPanel("Тестирование",
             testUI()
    )
  )
)
