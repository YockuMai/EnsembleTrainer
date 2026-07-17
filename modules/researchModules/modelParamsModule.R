# modules/researchModules/modelParamsModule.R

# =============================================================================
# Модуль настройки параметров моделей (только UI + сохранение параметров)
# =============================================================================

params_model <- function() {
  list(
    rf   = list(ntree = 500, mtry = floor(sqrt(5)), nodesize = 5, maxnodes = NULL, replace = TRUE),
    et   = list(ntree = 500, mtry = floor(sqrt(5)), nodesize = 5),
    gbm  = list(n.trees = 500, interaction.depth = 3, shrinkage = 0.1),
    xgb  = list(nrounds = 1000, max_depth = 6, eta = 0.1, subsample = 0.8),
    ada  = list(mfinal = 100, maxdepth = 2, coeflearn = "Breiman"),
    stack = list(
      base_models = c("rf", "gbm", "rpart", "knn", "lda"),
      meta_model = "glm",
      base_params = list(),
      meta_params = list()
    ),
    glm  = list(maxit = 100),
    knn  = list(k = 5),
    rpart = list(cp = 0.01, maxdepth = 30),
    nb   = list(fL = 0),
    lda  = list()
  )
}

stackable_models <- c(
  "Случайный лес" = "rf", "Extra Trees" = "et", "XGBoost" = "xgb",
  "AdaBoost" = "ada", "Логистическая регрессия" = "glm",
  "Наивный Байес" = "nb", "GBM" = "gbm", "Дерево решений" = "rpart",
  "k-NN" = "knn", "ЛДА" = "lda"
)

model_names <- c(
  "rf" = "Случайный лес", "et" = "Extra Trees", "gbm" = "GBM",
  "xgb" = "XGBoost", "ada" = "AdaBoost", "stack" = "Стекинг",
  "glm" = "Логистическая регрессия", "knn" = "k-NN",
  "rpart" = "Дерево решений", "nb" = "Наивный Байес", "lda" = "ЛДА"
)

# =============================================================================
# Функции рендеринга параметров для каждой модели
# =============================================================================
# Каждая принимает: ns, prefix (строка перед именем параметра), params, n_features

render_rf_params <- function(ns, prefix, params, n_features = 50) {
  max_mtry <- max(1, n_features)
  id <- function(x) ns(paste0(prefix, x))
  mtry_val <- params$mtry
  if (is.null(mtry_val) || is.na(mtry_val) || mtry_val > max_mtry) mtry_val <- floor(sqrt(max_mtry))
  tagList(
    numericInput(id("ntree"), "ntree (количество деревьев)",
                 value = params$ntree %||% 500, min = 10, max = 5000, step = 10),
    numericInput(id("mtry"), "mtry (число признаков для разбиения)",
                 value = mtry_val, min = 1, max = max_mtry, step = 1),
    numericInput(id("nodesize"), "nodesize (мин. размер листа)",
                 value = params$nodesize %||% 5, min = 1, max = 100, step = 1),
    numericInput(id("maxnodes"), "maxnodes (макс. число узлов, 0 = без ограничений)",
                 value = params$maxnodes %||% 0, min = 0, max = 2000, step = 1),
    selectInput(id("replace"), "replace (выборка с возвращением)",
                choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                selected = params$replace %||% TRUE)
  )
}

render_et_params <- function(ns, prefix, params, n_features = 50) {
  max_mtry <- max(1, n_features)
  id <- function(x) ns(paste0(prefix, x))
  mtry_val <- params$mtry
  if (is.null(mtry_val) || is.na(mtry_val) || mtry_val > max_mtry) mtry_val <- floor(sqrt(max_mtry))
  tagList(
    numericInput(id("ntree"), "num.trees (количество деревьев)",
                 value = params$ntree %||% 500, min = 10, max = 10000, step = 10),
    numericInput(id("mtry"), "mtry (число признаков для разбиения)",
                 value = mtry_val, min = 1, max = max_mtry, step = 1),
    numericInput(id("nodesize"), "min.node.size (мин. размер листа)",
                 value = params$nodesize %||% 1, min = 1, max = 100, step = 1)
  )
}

render_gbm_params <- function(ns, prefix, params, n_features = 50) {
  id <- function(x) ns(paste0(prefix, x))
  tagList(
    numericInput(id("n.trees"), "n.trees (количество деревьев)",
                 value = params$n.trees %||% 100, min = 10, max = 10000, step = 10),
    numericInput(id("interaction.depth"), "interaction.depth (глубина деревьев)",
                 value = params$interaction.depth %||% 3, min = 1, max = 100, step = 1),
    numericInput(id("shrinkage"), "shrinkage (скорость обучения)",
                 value = params$shrinkage %||% 0.1, min = 0.001, max = 1, step = 0.001)
  )
}

render_xgb_params <- function(ns, prefix, params, n_features = 50) {
  id <- function(x) ns(paste0(prefix, x))
  tagList(
    numericInput(id("nrounds"), "nrounds (количество деревьев)",
                 value = params$nrounds %||% 500, min = 1, max = 10000, step = 10),
    numericInput(id("max_depth"), "max_depth (максимальная глубина дерева)",
                 value = params$max_depth %||% 6, min = 1, max = 150, step = 1),
    numericInput(id("eta"), "eta (скорость обучения)",
                 value = params$eta %||% 0.3, min = 0.001, max = 1, step = 0.001),
    numericInput(id("subsample"), "subsample (доля строк для каждого дерева)",
                 value = params$subsample %||% 1, min = 0.1, max = 1, step = 0.05)
  )
}

render_ada_params <- function(ns, prefix, params, n_features = 50) {
  id <- function(x) ns(paste0(prefix, x))
  tagList(
    numericInput(id("mfinal"), "mfinal (кол-во итераций)",
                 value = params$mfinal %||% 100, min = 10, max = 5000, step = 10),
    numericInput(id("maxdepth"), "maxdepth (глубина дерева)",
                 value = params$maxdepth %||% 2, min = 1, max = 50, step = 1),
    selectInput(id("coeflearn"), "coeflearn",
                choices = c("Breiman", "Freund", "Zhu"),
                selected = params$coeflearn %||% "Breiman")
  )
}

render_glm_params <- function(ns, prefix, params, n_features = 50) {
  id <- function(x) ns(paste0(prefix, x))
  tagList(
    numericInput(id("maxit"), "maxit", value = params$maxit %||% 100, min = 1, max = 500, step = 10)
  )
}

render_knn_params <- function(ns, prefix, params, n_features = 50) {
  id <- function(x) ns(paste0(prefix, x))
  tagList(
    numericInput(id("k"), "k (кол-во соседей)", value = params$k %||% 5, min = 1, max = 50, step = 1)
  )
}

render_rpart_params <- function(ns, prefix, params, n_features = 50) {
  id <- function(x) ns(paste0(prefix, x))
  tagList(
    numericInput(id("cp"), "cp (параметр сложности)",
                 value = params$cp %||% 0.01, min = 0.001, max = 1, step = 0.001),
    numericInput(id("maxdepth"), "maxdepth", value = params$maxdepth %||% 30, min = 1, max = 500, step = 1)
  )
}

render_nb_params <- function(ns, prefix, params, n_features = 50) {
  id <- function(x) ns(paste0(prefix, x))
  tagList(
    numericInput(id("fL"), "fL (сглаживание Лапласа)",
                 value = params$fL %||% 0, min = 0, max = 100, step = 0.5)
  )
}

render_lda_params <- function(ns, prefix, params, n_features = 50) {
  p("Параметры для настройки отсутствуют")
}

# Диспетчер
render_model_params <- function(ns, prefix, model_id, params, n_features = 50) {
  switch(model_id,
    "rf"    = render_rf_params(ns, prefix, params, n_features),
    "et"    = render_et_params(ns, prefix, params, n_features),
    "gbm"   = render_gbm_params(ns, prefix, params, n_features),
    "xgb"   = render_xgb_params(ns, prefix, params, n_features),
    "ada"   = render_ada_params(ns, prefix, params, n_features),
    "glm"   = render_glm_params(ns, prefix, params, n_features),
    "knn"   = render_knn_params(ns, prefix, params, n_features),
    "rpart" = render_rpart_params(ns, prefix, params, n_features),
    "nb"    = render_nb_params(ns, prefix, params, n_features),
    "lda"   = render_lda_params(ns, prefix, params, n_features),
    stop(paste("Неизвестная модель:", model_id))
  )
}

# =============================================================================
# UI
# =============================================================================

modelParamsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 3,
        wellPanel(
          h4("Выберите модели"),
          uiOutput(ns("models_checkbox_ui")),
          hr(),
          uiOutput(ns("target_var_ui")),
          br(),
          uiOutput(ns("train_ratio_ui"))
        )
      ),
      column(width = 9,
        uiOutput(ns("params_tabs"))
      )
    )
  )
}

# =============================================================================
# Серверная часть
# =============================================================================

modelParamsServer <- function(id, session_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      if (is.null(session_data$model_params)) {
        session_data$model_params <- params_model()
        session_data$model_params$selected_models <- character(0)
        session_data$model_params$train_ratio <- 0.8
      }
    })
    
    get_current_data <- function() {
      if (!is.null(session_data$preprocess_path) && file.exists(session_data$preprocess_path))
        return(load_data_frame(session_data$preprocess_path))
      if (!is.null(session_data$original_data_path) && file.exists(session_data$original_data_path))
        return(load_data_frame(session_data$original_data_path))
      return(NULL)
    }
    
    data_reactive <- reactive({ get_current_data() }) %>% bindEvent(
      session_data$original_data_path, session_data$preprocess_path, ignoreNULL = FALSE
    )
    
    output$models_checkbox_ui <- renderUI({
      req(session_data$model_params)
      checkboxGroupInput(ns("models_selected"), NULL,
        choices = c("Случайный лес" = "rf", "Extra Trees" = "et", "GBM" = "gbm",
                    "XGBoost" = "xgb", "AdaBoost" = "ada", "Стекинг" = "stack",
                    "Логистическая регрессия" = "glm", "k-ближайшие соседи" = "knn",
                    "Дерево решений" = "rpart", "Наивный Байес" = "nb", "ЛДА" = "lda"),
        selected = session_data$model_params$selected_models)
    })
    
    output$train_ratio_ui <- renderUI({
      req(session_data$model_params)
      sliderInput(ns("train_ratio"), "Доля обучающей выборки",
                  min = 0.5, max = 0.9, value = session_data$model_params$train_ratio, step = 0.05)
    })
    
    output$target_var_ui <- renderUI({
      df <- data_reactive()
      if (is.null(df)) return(div(class = "alert alert-warning", "Данные не загружены."))
      cat_cols <- names(df)[sapply(df, is.factor)]
      if (length(cat_cols) == 0) return(div(class = "alert alert-warning", "Нет факторных столбцов."))
      selected_target <- session_data$model_params$target_var
      if (is.null(selected_target) || !(selected_target %in% cat_cols)) selected_target <- cat_cols[1]
      selectInput(ns("target_var"), "Целевая переменная", choices = cat_cols, selected = selected_target)
    })
    
    get_n_features <- function() {
      df <- data_reactive()
      if (!is.null(df)) ncol(df) - 1 else 50
    }
    
    output$params_tabs <- renderUI({
      selected <- input$models_selected
      if (is.null(selected) || length(selected) == 0)
        return(div(class = "alert alert-info", style = "margin-top: 20px;", h4("Выберите хотя бы одну модель")))
      
      tabs <- lapply(selected, function(mid) {
        if (mid == "stack") {
          tabPanel("Стекинг", value = "stack", br(), uiOutput(ns("params_stack")))
        } else {
          tabPanel(model_names[mid] %||% mid, value = mid, br(), uiOutput(ns(paste0("params_", mid))))
        }
      })
      do.call(tabsetPanel, c(id = ns("model_tabs"), tabs))
    })
    
    # ---- Параметры обычных моделей ----
    # prefix = model_id + "_" чтобы ID совпадали с observer-ами (rf_ntree, et_mtry, ...)
    output$params_rf    <- renderUI({ render_model_params(ns, "rf_",    "rf",    session_data$model_params$params$rf,    get_n_features()) })
    output$params_et    <- renderUI({ render_model_params(ns, "et_",    "et",    session_data$model_params$params$et,    get_n_features()) })
    output$params_gbm   <- renderUI({ render_model_params(ns, "gbm_",   "gbm",   session_data$model_params$params$gbm,   get_n_features()) })
    output$params_xgb   <- renderUI({ render_model_params(ns, "xgb_",   "xgb",   session_data$model_params$params$xgb,   get_n_features()) })
    output$params_ada   <- renderUI({ render_model_params(ns, "ada_",   "ada",   session_data$model_params$params$ada,   get_n_features()) })
    output$params_glm   <- renderUI({ render_model_params(ns, "glm_",   "glm",   session_data$model_params$params$glm,   get_n_features()) })
    output$params_knn   <- renderUI({ render_model_params(ns, "knn_",   "knn",   session_data$model_params$params$knn,   get_n_features()) })
    output$params_rpart <- renderUI({ render_model_params(ns, "rpart_", "rpart", session_data$model_params$params$rpart, get_n_features()) })
    output$params_nb    <- renderUI({ render_model_params(ns, "nb_",    "nb",    session_data$model_params$params$nb,    get_n_features()) })
    output$params_lda   <- renderUI({ render_model_params(ns, "lda_",   "lda",   session_data$model_params$params$lda,   get_n_features()) })
    
    # ---- Параметры стекинга ----
    output$params_stack <- renderUI({
      p <- session_data$model_params$params$stack
      base_models <- p$base_models %||% character(0)
      
      base_tabs <- lapply(base_models, function(bm) {
        bm_params <- p$base_params[[bm]] %||% params_model()[[bm]] %||% list()
        tabPanel(model_names[bm] %||% bm, value = bm, br(),
                 render_model_params(ns, paste0("stack_", bm, "_"), bm, bm_params, get_n_features()))
      })
      
      # Параметры метамодели (текущей выбранной)
      meta_params <- p$meta_params %||% params_model()[[p$meta_model]] %||% list()
      
      tagList(
        h5("Настройка стекинга"),
        fluidRow(
          column(6, checkboxGroupInput(ns("stack_base_models"), "Выберите базовые модели",
                                       choices = stackable_models, selected = base_models)),
          column(6, radioButtons(ns("stack_meta_model"), "Выберите метамодель",
                                 choices = stackable_models, selected = p$meta_model %||% "glm"))
        ),
        hr(),
        h5("Параметры метамодели"),
        render_model_params(ns, "stack_meta_", p$meta_model %||% "glm", meta_params, get_n_features()),
        hr(),
        h5("Параметры базовых моделей"),
        if (length(base_models) > 0) {
          do.call(tabsetPanel, c(id = ns("stack_base_tabs"), base_tabs))
        } else {
          p("Выберите хотя бы одну базовую модель.")
        }
      )
    })
    
    # ---- Синхронизация ----
    observeEvent(input$models_selected, {
      session_data$model_params$selected_models <- input$models_selected
      for (m in input$models_selected) {
        if (!(m %in% names(session_data$model_params$params)))
          session_data$model_params$params[[m]] <- params_model()[[m]]
      }
    })
    
    observeEvent(input$target_var,  { session_data$model_params$target_var <- input$target_var })
    observeEvent(input$train_ratio, { session_data$model_params$train_ratio <- input$train_ratio })
    
    # Параметры обычных моделей
    observeEvent(input$rf_mtry,     { session_data$model_params$params$rf$mtry <- input$rf_mtry })
    observeEvent(input$rf_nodesize, { session_data$model_params$params$rf$nodesize <- input$rf_nodesize })
    observeEvent(input$rf_ntree,    { session_data$model_params$params$rf$ntree <- input$rf_ntree })
    observeEvent(input$rf_maxnodes, { session_data$model_params$params$rf$maxnodes <- input$rf_maxnodes })
    observeEvent(input$rf_replace,  { session_data$model_params$params$rf$replace <- as.logical(input$rf_replace) })
    observeEvent(input$et_mtry,     { session_data$model_params$params$et$mtry <- input$et_mtry })
    observeEvent(input$et_nodesize, { session_data$model_params$params$et$nodesize <- input$et_nodesize })
    observeEvent(input$et_ntree,    { session_data$model_params$params$et$ntree <- input$et_ntree })
    observeEvent(input$gbm_n.trees, { session_data$model_params$params$gbm$n.trees <- input$gbm_n.trees })
    observeEvent(input$gbm_interaction.depth, { session_data$model_params$params$gbm$interaction.depth <- input$gbm_interaction.depth })
    observeEvent(input$gbm_shrinkage, { session_data$model_params$params$gbm$shrinkage <- input$gbm_shrinkage })
    observeEvent(input$xgb_nrounds,   { session_data$model_params$params$xgb$nrounds <- input$xgb_nrounds })
    observeEvent(input$xgb_max_depth, { session_data$model_params$params$xgb$max_depth <- input$xgb_max_depth })
    observeEvent(input$xgb_eta,       { session_data$model_params$params$xgb$eta <- input$xgb_eta })
    observeEvent(input$xgb_subsample, { session_data$model_params$params$xgb$subsample <- input$xgb_subsample })
    observeEvent(input$ada_mfinal,    { session_data$model_params$params$ada$mfinal <- input$ada_mfinal })
    observeEvent(input$ada_maxdepth,  { session_data$model_params$params$ada$maxdepth <- input$ada_maxdepth })
    observeEvent(input$ada_coeflearn, { session_data$model_params$params$ada$coeflearn <- input$ada_coeflearn })
    observeEvent(input$glm_maxit,     { session_data$model_params$params$glm$maxit <- input$glm_maxit })
    observeEvent(input$knn_k,         { session_data$model_params$params$knn$k <- input$knn_k })
    observeEvent(input$rpart_cp,      { session_data$model_params$params$rpart$cp <- input$rpart_cp })
    observeEvent(input$rpart_maxdepth,{ session_data$model_params$params$rpart$maxdepth <- input$rpart_maxdepth })
    observeEvent(input$nb_fL,         { session_data$model_params$params$nb$fL <- input$nb_fL })
    
    # Стекинг
    observeEvent(input$stack_base_models, {
      old_base <- session_data$model_params$params$stack$base_models %||% character(0)
      new_base <- input$stack_base_models
      session_data$model_params$params$stack$base_models <- new_base
      for (bm in new_base) {
        if (is.null(session_data$model_params$params$stack$base_params[[bm]]))
          session_data$model_params$params$stack$base_params[[bm]] <- params_model()[[bm]] %||% list()
      }
      for (bm in setdiff(old_base, new_base))
        session_data$model_params$params$stack$base_params[[bm]] <- NULL
    })
    
    observeEvent(input$stack_meta_model, {
      session_data$model_params$params$stack$meta_model <- input$stack_meta_model
      # Всегда переинициализируем параметры для новой метамодели
      # (у разных моделей разные наборы параметров)
      session_data$model_params$params$stack$meta_params <- params_model()[[input$stack_meta_model]] %||% list()
    })
    
    # Параметры базовых моделей стекинга
    stack_param_defs <- list(
      c("rf", "ntree"), c("rf", "mtry"), c("rf", "nodesize"), c("rf", "maxnodes"), c("rf", "replace"),
      c("et", "ntree"), c("et", "mtry"), c("et", "nodesize"),
      c("gbm", "n.trees"), c("gbm", "interaction.depth"), c("gbm", "shrinkage"),
      c("xgb", "nrounds"), c("xgb", "max_depth"), c("xgb", "eta"), c("xgb", "subsample"),
      c("ada", "mfinal"), c("ada", "maxdepth"), c("ada", "coeflearn"),
      c("glm", "maxit"), c("knn", "k"), c("rpart", "cp"), c("rpart", "maxdepth"), c("nb", "fL")
    )
    
    for (def in stack_param_defs) {
      local({
        .bm <- def[1]
        .pname <- def[2]
        .input_id <- paste0("stack_", .bm, "_", .pname)
        observeEvent(input[[.input_id]], {
          val <- input[[.input_id]]
          if (!is.null(val)) {
            if (.pname == "replace") {
              session_data$model_params$params$stack$base_params[[.bm]][[.pname]] <- as.logical(val)
            } else {
              session_data$model_params$params$stack$base_params[[.bm]][[.pname]] <- val
            }
          }
        }, ignoreNULL = TRUE, ignoreInit = TRUE)
      })
    }
    
    # Параметры метамодели стекинга (префикс "stack_meta_")
    meta_param_defs <- list(
      c("ntree"), c("mtry"), c("nodesize"), c("maxnodes"), c("replace"),
      c("n.trees"), c("interaction.depth"), c("shrinkage"),
      c("nrounds"), c("max_depth"), c("eta"), c("subsample"),
      c("mfinal"), c("maxdepth"), c("coeflearn"),
      c("maxit"), c("k"), c("cp"), c("fL"), c("minsplit")
    )
    
    for (def in meta_param_defs) {
      local({
        .pname <- def[1]
        .input_id <- paste0("stack_meta_", .pname)
        observeEvent(input[[.input_id]], {
          val <- input[[.input_id]]
          if (!is.null(val)) {
            if (.pname == "replace") {
              session_data$model_params$params$stack$meta_params[[.pname]] <- as.logical(val)
            } else {
              session_data$model_params$params$stack$meta_params[[.pname]] <- val
            }
          }
        }, ignoreNULL = TRUE, ignoreInit = TRUE)
      })
    }
    
  })
}