source("R/preprocessData.R")

preprocessServer <- function(id, session_data) {
  moduleServer(id, function(input, output, session) {

    preprocessData <- reactiveVal(NULL)

    # Инициализация processed_data копией original_data
    observe({
      currentData <- session_data()
      if (!is.null(currentData$original_data) && is.null(currentData$processed_data)) {
        currentData$processed_data <- currentData$original_data
        session_data(currentData)
        preprocessData(PreprocessData$new(currentData$processed_data))
      }
    })

    
  })
}
