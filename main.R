library(shiny)
source("server/server.R")
source("ui/ui.R")

options(shiny.host = "127.0.0.1")
options(shiny.port = 6698)

shinyApp(ui = ui, server = server)
