library(shiny)
source("server/server.R")
source("ui/ui.R")

# Запуск Shiny приложения
shinyApp(ui = ui, server = server)
