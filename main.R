library(shiny)
library(caret)
library(caretEnsemble)
library(dplyr)
library(DT)
library(DBI)
library(RSQLite)
library(bcrypt)

source("modules/mainModule.R")

options(shiny.host = "127.0.0.1")
options(shiny.port = 6698)

options(shiny.reactlog = TRUE)

server <- create_server()
ui <- create_ui()

app <- shinyApp(ui = ui, server = server)

shiny::runApp(app)
