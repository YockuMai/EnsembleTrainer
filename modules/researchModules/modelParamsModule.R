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
    stack = list(base_models = c("rf", "gbm", "rpart", "knn", "lda"), meta_model = "glm"),
    glm  = list(maxit = 100),
    knn  = list(k = 5),
    rpart = list(cp = 0.01, maxdepth = 30),
    nb   = list(fL = 0),
    lda  = list()
  )
}


modelParamsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # Левая панель со списком моделей и общими настройками
      column(
        width = 3,
        wellPanel(
          h4("Выберите модели"),
          uiOutput(ns("models_checkbox_ui")),
          hr(),
          uiOutput(ns("target_var_ui")),
          br(),
          uiOutput(ns("train_ratio_ui"))
          # Кнопка обучения УДАЛЕНА
        )
      ),
      # Правая панель с вкладками параметров
      column(
        width = 9,
        uiOutput(ns("params_tabs"))
      )
    )
  )
}

# -----------------------------------------------------------------------------
# Серверная часть модуля (только синхронизация параметров с session_data)
# -----------------------------------------------------------------------------

modelParamsServer <- function(id, session_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---- Инициализация параметров в session_data (выполняется один раз) ----
    observe({
      if (is.null(session_data$model_params)) {
        session_data$model_params <- params_model()
        session_data$model_params$selected_models <- character(0)
        session_data$model_params$train_ratio <- 0.8
      }
    })
    
    # ---- Флаг для однократной загрузки статических элементов UI ----
    initialized <- reactiveVal(FALSE)
    
    # ---- Загрузка параметров в UI при первом запуске ----
    # Рендерим чекбоксы сразу с нужными значениями из сессии
    output$models_checkbox_ui <- renderUI({
      # Ждем, пока структура данных точно создастся
      req(session_data$model_params) 
      
      checkboxGroupInput(
        inputId = ns("models_selected"), # Внутри renderUI обязательно пишем ns()
        label = NULL,
        choices = c(
          "Случайный лес"          = "rf",
          "Extra Trees"            = "et",
          "GBM"                    = "gbm",
          "XGBoost"                = "xgb",
          "AdaBoost"               = "ada",
          "Стекинг"                = "stack",
          "Логистическая регрессия"= "glm",
          "k-ближайшие соседи"     = "knn",
          "Дерево решений"         = "rpart",
          "Наивный Байес"          = "nb",
          "ЛДА"                    = "lda"
        ),
        selected = session_data$model_params$selected_models
      )
    })
    
    # Рендерим слайдер
    output$train_ratio_ui <- renderUI({
      req(session_data$model_params)
      sliderInput(
        inputId = ns("train_ratio"),
        label = "Доля обучающей выборки",
        min = 0.1, max = 0.9, 
        value = session_data$model_params$train_ratio, 
        step = 0.05
      )
    })
    
    # ---- Данные для выбора целевой переменной ----
    data_reactive <- reactive({
      req(session_data$preprocess_obj)
      session_data$preprocess_obj
    })
    
    # ---- Выпадающий список целевой переменной (динамический) ----
    output$target_var_ui <- renderUI({
      df <- data_reactive()
      req(df)
      
      cat_cols <- names(df)[sapply(df, is.factor)]
      if (length(cat_cols) == 0) {
        return(
          div(
            class = "alert alert-warning",
            "В загруженных данных нет категориальных (факторных) столбцов. 
             Целевая переменная должна быть фактором."
          )
        )
      }
      
      selected_target <- session_data$model_params$target_var
      if (is.null(selected_target) || !(selected_target %in% cat_cols)) {
        selected_target <- cat_cols[1]
      }
      
      selectInput(
        inputId = ns("target_var"),
        label = "Целевая переменная",
        choices = cat_cols,
        selected = selected_target
      )
    })
    
    # ---- Динамические вкладки параметров ----
    output$params_tabs <- renderUI({
      selected <- input$models_selected
      if (is.null(selected) || length(selected) == 0) {
        return(
          div(
            class = "alert alert-info",
            style = "margin-top: 20px;",
            h4("Выберите хотя бы одну модель для настройки")
          )
        )
      }
      
      tabs <- lapply(selected, function(model_id) {
        tabPanel(
          title = switch(
            model_id,
            "rf"   = "Случайный лес",
            "et"   = "Extra Trees",
            "gbm"  = "GBM",
            "xgb"  = "XGBoost",
            "ada"  = "AdaBoost",
            "stack"= "Стекинг",
            "glm"  = "Логистическая регрессия",
            "knn"  = "k-NN",
            "rpart"= "Дерево решений",
            "nb"   = "Наивный Байес",
            "lda"  = "ЛДА"
          ),
          value = model_id,
          br(),
          uiOutput(ns(paste0("params_", model_id)))
        )
      })
      
      do.call(tabsetPanel, c(id = ns("model_tabs"), tabs))
    })
    
    # ---- Рендеринг параметров для каждой модели ----
    output$params_rf <- renderUI({
      p <- session_data$model_params$params$rf
      max_mtry <- if (!is.null(session_data$preprocess_obj)) ncol(session_data$preprocess_obj) - 1 else 50
      tagList(
        numericInput(ns("rf_ntree"), "ntree (количество деревьев)",
                     value = p$ntree, min = 10, max = 5000, step = 10),
        numericInput(ns("rf_mtry"), 
                     label = "mtry (число признаков для разбиения)",
                     value = p$mtry %||% round(sqrt(ncol(session_data$preprocess_obj) - 1)),
                     min = 1, max = max_mtry, step = 1),
        numericInput(ns("rf_nodesize"), "nodesize (мин. размер листа)",
                     value = p$nodesize, min = 1, max = 100, step = 1),
        numericInput(ns("rf_maxnodes"), "maxnodes (макс. число узлов в дереве, 0 = без ограничений)",
                     value = p$maxnodes %||% 0, min = 0, max = 2000, step = 1),
        selectInput(ns("rf_replace"), "replace (выборка с возвращением)",
                    choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                    selected = p$replace)
      )
    })
    
   output$params_et <- renderUI({
      p <- session_data$model_params$params$et
      # Определяем максимальное mtry динамически (если доступны данные)
      max_mtry <- if (!is.null(session_data$preprocess_obj)) ncol(session_data$preprocess_obj) - 1 else 50

      tagList(
        numericInput(ns("et_ntree"), 
                     label = "num.trees (количество деревьев)",
                     value = p$ntree %||% 500,
                     min = 10, max = 10000, step = 10),

        numericInput(ns("et_mtry"), 
                     label = "mtry (число признаков для разбиения)",
                     value = p$mtry %||% round(sqrt(ncol(session_data$preprocess_obj) - 1)),
                     min = 1, max = max_mtry, step = 1),

        numericInput(ns("et_nodesize"), 
                     label = "min.node.size (мин. размер листа)",
                     value = p$nodesize %||% 1,
                     min = 1, max = 100, step = 1)
      )
    })
    
    output$params_gbm <- renderUI({
      p <- session_data$model_params$params$gbm
      tagList(
        numericInput(ns("gbm_n.trees"), 
                     label = "n.trees (количество деревьев)", 
                     value = p$n.trees %||% 100, 
                     min = 10, max = 10000, step = 10),

        numericInput(ns("gbm_interaction.depth"), 
                     label = "interaction.depth (глубина деревьев)", 
                     value = p$interaction.depth %||% 3, 
                     min = 1, max = 100, step = 1),

        numericInput(ns("gbm_shrinkage"), 
                     label = "shrinkage (скорость обучения)", 
                     value = p$shrinkage %||% 0.1, 
                     min = 0.001, max = 1, step = 0.001)
      )
    })
    
    output$params_xgb <- renderUI({
      p <- session_data$model_params$params$xgb
      tagList(
        numericInput(ns("xgb_nrounds"), 
                     label = "nrounds (количество деревьев)", 
                     value = p$nrounds %||% 500, 
                     min = 1, max = 10000, step = 10),

        numericInput(ns("xgb_max_depth"), 
                     label = "max_depth (максимальная глубина дерева)", 
                     value = p$max_depth %||% 6, 
                     min = 1, max = 150, step = 1),

        numericInput(ns("xgb_eta"), 
                     label = "eta (скорость обучения, шаг уменьшения весов)", 
                     value = p$eta %||% 0.3, 
                     min = 0.001, max = 1, step = 0.001),

        numericInput(ns("xgb_subsample"), 
                     label = "subsample (доля строк для каждого дерева)", 
                     value = p$subsample %||% 1, 
                     min = 0.1, max = 1, step = 0.05)
      )
    })
    
    output$params_ada <- renderUI({
      p <- session_data$model_params$params$ada
      tagList(
        numericInput(ns("ada_mfinal"), "mfinal (кол-во итераций)", value = p$mfinal, min = 10, max = 5000, step = 10),
        numericInput(ns("ada_maxdepth"), "maxdepth (глубина дерева)", value = p$maxdepth, min = 1, max = 50, step = 1),
        selectInput(ns("ada_coeflearn"), "coeflearn", 
                    choices = c("Breiman", "Freund", "Zhu"), selected = p$coeflearn)
      )
    })
    
    output$params_stack <- renderUI({
      p <- session_data$model_params$params$stack
      tagList(
        h5("Настройка стекинга"),
        checkboxGroupInput(
          ns("stack_base_models"),
          label = "Выберите базовые модели",
          choices = c(
            "Случайный лес" = "rf",
            "Extra Trees" = "et",
            "XGBoost" = "xgb",
            "AdaBoost" = "ada",
            "Логистическая регрессия" = "glm",
            "Наивный Байес" = "nb",
            "GBM" = "gbm",
            "Дерево решений" = "rpart",
            "k-NN" = "knn",
            "ЛДА" = "lda"
          ),
          selected = p$base_models
        ),
        radioButtons(
          ns("stack_meta_model"),
          label = "Выберите метамодель",
          choices = c(
            "Случайный лес" = "rf",
            "Extra Trees" = "et",
            "XGBoost" = "xgb",
            "AdaBoost" = "ada",
            "Логистическая регрессия" = "glm",
            "Наивный Байес" = "nb",
            "GBM" = "gbm",
            "Дерево решений" = "rpart",
            "k-NN" = "knn",
            "ЛДА" = "lda"
          ),
          selected = p$meta_model
        ),
        p("Детальная настройка параметров базовых моделей будет добавлена в следующих версиях.")
      )
    })
    
    output$params_glm <- renderUI({
      p <- session_data$model_params$params$glm
      tagList(
        numericInput(ns("glm_maxit"), "maxit", value = p$maxit, min = 1, max = 500, step = 10)
      )
    })
    
    output$params_knn <- renderUI({
      p <- session_data$model_params$params$knn
      tagList(
        numericInput(ns("knn_k"), "k (кол-во соседей)", value = p$k, min = 1, max = 50, step = 1),
      )
    })
    
    output$params_rpart <- renderUI({
      p <- session_data$model_params$params$rpart
      tagList(
        numericInput(ns("rpart_cp"), "cp (параметр сложности)", value = p$cp, min = 0.001, max = 1, step = 0.001),
        numericInput(ns("rpart_maxdepth"), "maxdepth", value = p$maxdepth, min = 1, max = 500, step = 1)
      )
    })
    
    output$params_nb <- renderUI({
      p <- session_data$model_params$params$nb
      tagList(
        numericInput(ns("nb_fL"), "fL (сглаживание Лапласа)", value = p$fL, min = 0, max = 100, step = 0.5)
      )
    })
    
    output$params_lda <- renderUI({
      p <- session_data$model_params$params$lda
      p("Параметры для настройки отсутствуют")
    })
    
    # ---- Синхронизация изменений: запись в session_data ----
    
    observeEvent(input$models_selected, {
      session_data$model_params$selected_models <- input$models_selected
      for (m in input$models_selected) {
        if (!(m %in% names(session_data$model_params$params))) {
          default_params <- params_model()[[m]]
          session_data$model_params$params[[m]] <- default_params
        }
      }
    })
    
    observeEvent(input$target_var, {
      session_data$model_params$target_var <- input$target_var
    })
    
    observeEvent(input$train_ratio, {
      session_data$model_params$train_ratio <- input$train_ratio
    })
    
    # Параметры для каждой модели
    observeEvent(input$rf_mtry, { session_data$model_params$params$rf$mtry <- input$rf_mtry })
    observeEvent(input$rf_nodesize, { session_data$model_params$params$rf$nodesize <- input$rf_nodesize })
    observeEvent(input$rf_ntree, { session_data$model_params$params$rf$ntree <- input$rf_ntree })
    
    observeEvent(input$et_mtry, { session_data$model_params$params$et$mtry <- input$et_mtry })
    observeEvent(input$et_nodesize, { session_data$model_params$params$et$nodesize <- input$et_nodesize })
    observeEvent(input$et_ntree, { session_data$model_params$params$et$ntree <- input$et_ntree })
    
    observeEvent(input$gbm_depth, { session_data$model_params$params$gbm$interaction.depth <- input$gbm_depth })
    observeEvent(input$gbm_shrinkage, { session_data$model_params$params$gbm$shrinkage <- input$gbm_shrinkage })
    observeEvent(input$gbm_ntrees, { session_data$model_params$params$gbm$n.trees <- input$gbm_ntrees })
    observeEvent(input$gbm_bag, { session_data$model_params$params$gbm$bag.fraction <- input$gbm_bag })
    
    observeEvent(input$xgb_eta, { session_data$model_params$params$xgb$eta <- input$xgb_eta })
    observeEvent(input$xgb_depth, { session_data$model_params$params$xgb$max_depth <- input$xgb_depth })
    observeEvent(input$xgb_subsample, { session_data$model_params$params$xgb$subsample <- input$xgb_subsample })
    observeEvent(input$xgb_colsample, { session_data$model_params$params$xgb$colsample_bytree <- input$xgb_colsample })
    observeEvent(input$xgb_nrounds, { session_data$model_params$params$xgb$nrounds <- input$xgb_nrounds })
    
    observeEvent(input$ada_mfinal, { session_data$model_params$params$ada$mfinal <- input$ada_mfinal })
    observeEvent(input$ada_maxdepth, { session_data$model_params$params$ada$maxdepth <- input$ada_maxdepth })
    observeEvent(input$ada_coeflearn, { session_data$model_params$params$ada$coeflearn <- input$ada_coeflearn })
    
    observeEvent(input$stack_base_models, { session_data$model_params$params$stack$base_models <- input$stack_base_models })
    observeEvent(input$stack_meta_model, { session_data$model_params$params$stack$meta_model <- input$stack_meta_model })
    
    observeEvent(input$glm_family, { session_data$model_params$params$glm$family <- input$glm_family })
    observeEvent(input$glm_maxit, { session_data$model_params$params$glm$maxit <- input$glm_maxit })
    
    observeEvent(input$knn_k, { session_data$model_params$params$knn$k <- input$knn_k })
    observeEvent(input$knn_kernel, { session_data$model_params$params$knn$kernel <- input$knn_kernel })
    
    observeEvent(input$rpart_cp, { session_data$model_params$params$rpart$cp <- input$rpart_cp })
    observeEvent(input$rpart_minsplit, { session_data$model_params$params$rpart$minsplit <- input$rpart_minsplit })
    observeEvent(input$rpart_maxdepth, { session_data$model_params$params$rpart$maxdepth <- input$rpart_maxdepth })
    
    observeEvent(input$nb_use_kernel, { session_data$model_params$params$nb$use_kernel <- as.logical(input$nb_use_kernel) })
    observeEvent(input$nb_fL, { session_data$model_params$params$nb$fL <- input$nb_fL })
    
    observeEvent(input$lda_method, { session_data$model_params$params$lda$method <- input$lda_method })
    observeEvent(input$lda_prior, { session_data$model_params$params$lda$prior <- input$lda_prior })
    
    # ---- Модуль ничего не возвращает, все данные доступны через session_data ----
  })
}