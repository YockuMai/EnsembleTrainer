library(shiny)
library(DT)
library(cookies)
source("server/loadDataServer.R")

server <- function(input, output, session) {
  loadDataServer("load")
}
