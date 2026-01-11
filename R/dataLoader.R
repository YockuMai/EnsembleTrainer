library(R6)

DataLoader <- R6::R6Class("DataLoader",
  private = list(
    data = NULL,
    error_message = NULL
  ),

  public = list(
    initialize = function() {
      # Initialization code if needed
      private$data <- NULL
      private$error_message <- NULL
    },
    csvLoad = function(filepath, sep) {
      tryCatch({
        if (!file.exists(filepath)) {
          private$error_message <- "Файл не найден"
          return(NULL)
        }
        if (!grepl("\\.csv$", filepath, ignore.case = TRUE)) {
          private$error_message <- "Файл должен иметь расширение .csv"
          return(NULL)
        }
        data <- read.csv(filepath, stringsAsFactors = FALSE, sep=sep)
        if (nrow(data) == 0) {
          private$error_message <- "Файл пустой или не содержит данных"
          return(NULL)
        }
        private$data <- data
        private$error_message <- NULL
        return(TRUE)
      }, error = function(e) {
        private$error_message <- paste("Ошибка при загрузке файла:", e$message)
        return(NULL)
      })
    },
    getData = function() {
      return(private$data)
    },
    getError = function() {
      return(private$error_message)
    },
    hasData = function() {
      return(!is.null(private$data))
    }
  )
)
