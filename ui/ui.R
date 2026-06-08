source("ui/practicUI.R")
source("ui/lectionUI.R")
source("ui/testUI.R")

create_ui <- function() {
    ui <- fluidPage(
      titlePanel("Ensemble Trainer"),
      uiOutput("login_ui"),
    )
}