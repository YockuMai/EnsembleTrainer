# =============================================================================
# Модуль обучения моделей (UI + server) с сохранением в session_data
# =============================================================================

modelTrainingUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        div(
          style = "margin-bottom: 20px;",
          actionButton(
            inputId = ns("train_btn"),
            label = "Обучить выбранные модели",
            icon = icon("play"),
            class = "btn-success btn-lg"
          ),
          actionButton(
            inputId = ns("clear_btn"),
            label = "Очистить результаты",
            icon = icon("trash"),
            class = "btn-warning"
          )
        ),
        div(
          style = "margin-top: 20px;",
          uiOutput(ns("results_output"))
        )
      )
    )
  )
}

# -----------------------------------------------------------------------------
# Серверная часть модуля обучения
# -----------------------------------------------------------------------------

modelTrainingServer <- function(id, session_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---- Инициализация полей в session_data ----
    observe({
      if (is.null(session_data$trained_models)) {
        session_data$trained_models <- list()
      }
      if (is.null(session_data$training_results)) {
        session_data$training_results <- NULL
      }
    })

    # ---- Автоматическая очистка моделей при удалении всех данных ----
    observeEvent(c(session_data$original_data_path, session_data$preprocess_path), {
      # Если оба пути NULL и есть обученные модели - удаляем их
      if (is.null(session_data$original_data_path) && is.null(session_data$preprocess_path)) {
        if (length(session_data$trained_models) > 0) {
          # Удаляем файлы моделей
          for (meta in session_data$trained_models) {
            if (file.exists(meta$path)) file.remove(meta$path)
          }
          # Очищаем reactive-данные
          session_data$trained_models <- list()
          session_data$training_results <- NULL
          # Обновляем метаданные в SQLite
          user_id <- session_data$user_id
          if (!is.null(user_id)) {
            save_user_data(user_id, session_data)
          }
          showNotification("Модели и результаты очищены в связи с удалением данных", type = "message")
        }
      }
    }, ignoreNULL = FALSE)
    
    # ---- Функция расчёта метрик ----
    calc_metrics <- function(probs, true) {
      classes <- levels(true)
      pred <- classes[apply(probs, 1, which.max)]
      acc <- mean(pred == true)
      
      prec <- recall <- f1 <- numeric(length(classes))
      for (i in seq_along(classes)) {
        tp <- sum(pred == classes[i] & true == classes[i])
        fp <- sum(pred == classes[i] & true != classes[i])
        fn <- sum(pred != classes[i] & true == classes[i])
        prec[i] <- ifelse(tp+fp==0, 0, tp/(tp+fp))
        recall[i] <- ifelse(tp+fn==0, 0, tp/(tp+fn))
        f1[i] <- ifelse(prec[i]+recall[i]==0, 0, 2*prec[i]*recall[i]/(prec[i]+recall[i]))
      }
      macro_prec <- mean(prec)
      macro_rec <- mean(recall)
      macro_f1 <- mean(f1)
      
      auc_vals <- numeric(length(classes))
      for (i in seq_along(classes)) {
        bin_true <- as.numeric(true == classes[i])
        if (length(unique(bin_true)) == 2) {
          auc_vals[i] <- pROC::auc(bin_true, probs[, i])
        } else auc_vals[i] <- NA
      }
      macro_auc <- mean(auc_vals, na.rm = TRUE)
      return(c(Accuracy = acc, Precision = macro_prec, Recall = macro_rec, F1 = macro_f1, AUC = macro_auc))
    }
    
    # ---- Обработчик кнопки "Обучить" ----
    observeEvent(input$train_btn, {
      user_id <- session_data$user_id
      req(user_id)   # обязательно, чтобы сохранять модели на диск
      
      # Загружаем данные с диска
      df <- NULL
      if (!is.null(session_data$preprocess_path) && file.exists(session_data$preprocess_path)) {
        df <- load_data_frame(session_data$preprocess_path)
      } else if (!is.null(session_data$original_data_path) && file.exists(session_data$original_data_path)) {
        df <- load_data_frame(session_data$original_data_path)
      }
      req(df, session_data$model_params)
      
      params_list <- session_data$model_params
      target <- params_list$target_var
      train_ratio <- params_list$train_ratio
      selected_models <- params_list$selected_models
      
      if (!is.factor(df[[target]])) {
        showNotification("Целевая переменная должна быть факторной", type = "error")
        return()
      }
      
      # Разделение на train/test
      set.seed(789)
      train_idx <- caret::createDataPartition(df[[target]], p = train_ratio, list = FALSE)
      train_data <- df[train_idx, ]
      test_data <- df[-train_idx, ]
      
      X_train <- train_data[, !(names(train_data) %in% target), drop = FALSE]
      y_train <- train_data[[target]]
      X_test <- test_data[, !(names(test_data) %in% target), drop = FALSE]
      y_test <- test_data[[target]]
      
      y_train_xgb <- as.numeric(y_train) - 1
      y_test_xgb <- as.numeric(y_test) - 1
      
      # ---- Инициализация списков для метаданных и результатов ----
      trained_models_meta <- list()   # <-- ОБЪЯВЛЯЕМ ЗДЕСЬ
      results_all <- list()
      
      withProgress(message = "Обучение моделей...", value = 0, {
        for (i in seq_along(selected_models)) {
          model_id <- selected_models[i]
          incProgress(1/length(selected_models), detail = paste("Модель:", model_id))
          
          params <- params_list$params[[model_id]]
          if (is.null(params)) {
            showNotification(paste("Параметры для модели", model_id, "не найдены"), type = "warning")
            next
          }
          
          result <- tryCatch({
            switch(
              model_id,
              # --- Случайный лес ---
              "rf" = {
                mod <- randomForest::randomForest(
                  x = X_train, y = y_train,
                  ntree = params$ntree,
                  mtry = params$mtry,
                  nodesize = params$nodesize,
                  importance = FALSE
                )
                probs <- predict(mod, X_test, type = "prob")
                metrics <- calc_metrics(probs, y_test)
                model_path <- save_model(mod, user_id, model_id)
                trained_models_meta[[model_id]] <- list(
                  path = model_path,
                  metrics = metrics,
                  params = params,
                  class = "randomForest"
                )
                rm(mod); gc()
                data.frame(Model = "RandomForest", t(metrics), stringsAsFactors = FALSE)
              },
              # --- Extra Trees ---
              "et" = {
                mod <- ranger::ranger(
                  dependent.variable.name = target,
                  data = train_data,
                  num.trees = params$ntree,
                  mtry = params$mtry,
                  min.node.size = params$nodesize,
                  splitrule = "extratrees",
                  replace = FALSE,
                  sample.fraction = 1,
                  probability = TRUE,
                  seed = 123
                )
                probs <- predict(mod, data = X_test)$predictions
                colnames(probs) <- levels(y_train)
                metrics <- calc_metrics(probs, y_test)
                model_path <- save_model(mod, user_id, model_id)
                trained_models_meta[[model_id]] <- list(
                  path = model_path,
                  metrics = metrics,
                  params = params,
                  class = "ranger"
                )
                rm(mod); gc()
                data.frame(Model = "ExtraTrees", t(metrics), stringsAsFactors = FALSE)
              },
              # --- GBM ---
              "gbm" = {
                mod <- gbm::gbm(
                  formula(paste(target, "~ .")),
                  data = train_data,
                  n.trees = params$n.trees,
                  interaction.depth = params$interaction.depth,
                  shrinkage = params$shrinkage,
                  bag.fraction = params$bag.fraction,
                  distribution = "multinomial",
                  verbose = FALSE
                )
                p_raw <- predict(mod, X_test, n.trees = params$n.trees, type = "response")
                if (is.array(p_raw) && length(dim(p_raw)) == 3) {
                  probs <- as.matrix(p_raw[,,1])
                } else {
                  probs <- as.matrix(p_raw)
                }
                colnames(probs) <- levels(y_train)
                metrics <- calc_metrics(probs, y_test)
                model_path <- save_model(mod, user_id, model_id)
                trained_models_meta[[model_id]] <- list(
                  path = model_path,
                  metrics = metrics,
                  params = params,
                  class = "gbm"
                )
                rm(mod); gc()
                data.frame(Model = "GBM", t(metrics), stringsAsFactors = FALSE)
              },
              # --- XGBoost ---
              "xgb" = {
                X_train_num <- as.data.frame(lapply(X_train, function(col) {
                  if (is.character(col)) as.numeric(as.factor(col)) else as.numeric(col)
                }))
                X_test_num <- as.data.frame(lapply(X_test, function(col) {
                  if (is.character(col)) as.numeric(as.factor(col)) else as.numeric(col)
                }))
                if (any(is.na(X_train_num)) || any(is.na(X_test_num))) {
                  stop("В данных есть пропуски (NA). XGBoost не может их обработать.")
                }
                X_train_mat <- as.matrix(X_train_num)
                X_test_mat <- as.matrix(X_test_num)
                
                dtrain <- xgb.DMatrix(X_train_mat, label = y_train_xgb)
                dtest <- xgb.DMatrix(X_test_mat, label = y_test_xgb)
                
                xgb_params <- list(
                  objective = "multi:softprob",
                  num_class = length(unique(y_train)),
                  eval_metric = "mlogloss",
                  max_depth = params$max_depth,
                  eta = params$eta,
                  subsample = params$subsample,
                  colsample_bytree = params$colsample_bytree,
                  min_child_weight = 1
                )
                mod <- xgb.train(xgb_params, dtrain, nrounds = params$nrounds, verbose = 0)
                probs <- predict(mod, dtest, reshape = TRUE)
                colnames(probs) <- levels(y_train)
                metrics <- calc_metrics(probs, y_test)
                model_path <- save_model(mod, user_id, model_id)
                trained_models_meta[[model_id]] <- list(
                  path = model_path,
                  metrics = metrics,
                  params = params,
                  class = "xgboost"
                )
                rm(mod); gc()
                data.frame(Model = "XGBoost", t(metrics), stringsAsFactors = FALSE)
              },
              # --- AdaBoost ---
              "ada" = {
                if (length(unique(y_train)) == 2) {
                  mod <- ada::ada(
                    formula(paste(target, "~ .")),
                    data = train_data,
                    iter = params$mfinal,
                    loss = "exponential"
                  )
                  probs <- predict(mod, newdata = test_data, type = "prob")
                  colnames(probs) <- levels(y_train)
                  metrics <- calc_metrics(probs, y_test)
                  model_path <- save_model(mod, user_id, model_id)
                  trained_models_meta[[model_id]] <- list(
                    path = model_path,
                    metrics = metrics,
                    params = params,
                    class = "ada"
                  )
                  rm(mod); gc()
                  data.frame(Model = "AdaBoost", t(metrics), stringsAsFactors = FALSE)
                } else {
                  mod <- adabag::boosting(
                    formula(paste(target, "~ .")),
                    data = train_data,
                    mfinal = params$mfinal,
                    control = rpart.control(maxdepth = params$maxdepth, minsplit = 10),
                    coeflearn = params$coeflearn
                  )
                  pred <- predict(mod, newdata = test_data)
                  probs <- pred$prob
                  colnames(probs) <- levels(y_train)
                  metrics <- calc_metrics(probs, y_test)
                  model_path <- save_model(mod, user_id, model_id)
                  trained_models_meta[[model_id]] <- list(
                    path = model_path,
                    metrics = metrics,
                    params = params,
                    class = "adabag"
                  )
                  rm(mod); gc()
                  data.frame(Model = "AdaBoost", t(metrics), stringsAsFactors = FALSE)
                }
              },
              # --- Стекинг ---
              "stack" = {
                base_models <- params$base_models
                meta_model <- params$meta_model
                
                if (length(base_models) == 0) {
                  stop("Не выбрано ни одной базовой модели для стекинга")
                }
                
                # Вспомогательная функция обучения и предсказания для базовой модели
                train_and_predict <- function(model_id, train_data, test_data, target) {
                  is_binary <- length(unique(train_data[[target]])) == 2
                  
                  switch(model_id,
                    "rf" = {
                      mod <- randomForest::randomForest(
                        formula(paste(target, "~ .")),
                        data = train_data,
                        ntree = 500,
                        mtry = floor(sqrt(ncol(train_data) - 1)),
                        nodesize = 5,
                        importance = FALSE
                      )
                      predict(mod, test_data, type = "prob")
                    },
                    "et" = {
                      mod <- ranger::ranger(
                        dependent.variable.name = target,
                        data = train_data,
                        num.trees = 500,
                        mtry = floor(sqrt(ncol(train_data) - 1)),
                        min.node.size = 5,
                        splitrule = "extratrees",
                        replace = FALSE,
                        sample.fraction = 1,
                        probability = TRUE,
                        seed = 123
                      )
                      probs <- predict(mod, data = test_data)$predictions
                      colnames(probs) <- levels(train_data[[target]])
                      probs
                    },
                    "gbm" = {
                      mod <- gbm::gbm(
                        formula(paste(target, "~ .")),
                        data = train_data,
                        n.trees = 500,
                        interaction.depth = 3,
                        shrinkage = 0.01,
                        bag.fraction = 0.5,
                        distribution = "multinomial",
                        verbose = FALSE
                      )
                      p_raw <- predict(mod, test_data, n.trees = 500, type = "response")
                      if (is.array(p_raw) && length(dim(p_raw)) == 3) {
                        probs <- as.matrix(p_raw[,,1])
                      } else {
                        probs <- as.matrix(p_raw)
                      }
                      colnames(probs) <- levels(train_data[[target]])
                      probs
                    },
                    "xgb" = {
                      to_numeric <- function(df) {
                        as.data.frame(lapply(df, function(col) {
                          if (is.character(col)) as.numeric(as.factor(col)) else as.numeric(col)
                        }))
                      }
                      train_num <- to_numeric(train_data[, !(names(train_data) %in% target)])
                      test_num <- to_numeric(test_data[, !(names(test_data) %in% target)])
                      train_num <- train_num[, colSums(is.na(train_num)) == 0, drop = FALSE]
                      test_num <- test_num[, colSums(is.na(test_num)) == 0, drop = FALSE]
                      
                      dtrain <- xgb.DMatrix(as.matrix(train_num), 
                                            label = as.numeric(train_data[[target]]) - 1)
                      dtest <- xgb.DMatrix(as.matrix(test_num))
                      
                      xgb_params <- list(
                        objective = if (is_binary) "binary:logistic" else "multi:softprob",
                        num_class = if (is_binary) 1 else length(unique(train_data[[target]])),
                        eval_metric = if (is_binary) "logloss" else "mlogloss",
                        max_depth = 6,
                        eta = 0.1,
                        subsample = 0.8,
                        colsample_bytree = 0.8,
                        min_child_weight = 1
                      )
                      if (!is_binary) xgb_params$num_class <- length(unique(train_data[[target]]))
                      
                      mod <- xgb.train(xgb_params, dtrain, nrounds = 100, verbose = 0)
                      probs <- predict(mod, dtest, reshape = TRUE)
                      if (is_binary) {
                        probs <- cbind(1 - probs, probs)
                      }
                      colnames(probs) <- levels(train_data[[target]])
                      probs
                    },
                    "ada" = {
                      if (is_binary) {
                        mod <- ada::ada(
                          formula(paste(target, "~ .")),
                          data = train_data,
                          iter = 100,
                          loss = "exponential"
                        )
                        probs <- predict(mod, newdata = test_data, type = "prob")
                      } else {
                        mod <- adabag::boosting(
                          formula(paste(target, "~ .")),
                          data = train_data,
                          mfinal = 100,
                          control = rpart.control(maxdepth = 2, minsplit = 10),
                          coeflearn = "Breiman"
                        )
                        pred <- predict(mod, newdata = test_data)
                        probs <- pred$prob
                      }
                      colnames(probs) <- levels(train_data[[target]])
                      probs
                    },
                    "glm" = {
                      if (is_binary) {
                        mod <- glm(
                          formula(paste(target, "~ .")),
                          data = train_data,
                          family = binomial(link = "logit")
                        )
                        probs_positive <- predict(mod, test_data, type = "response")
                        probs <- cbind(1 - probs_positive, probs_positive)
                      } else {
                        mod <- nnet::multinom(
                          formula(paste(target, "~ .")),
                          data = train_data,
                          maxit = 100,
                          trace = FALSE
                        )
                        probs <- predict(mod, test_data, type = "probs")
                      }
                      colnames(probs) <- levels(train_data[[target]])
                      probs
                    },
                    "nb" = {
                      mod <- e1071::naiveBayes(
                        formula(paste(target, "~ .")),
                        data = train_data,
                        usekernel = FALSE,
                        fL = 0
                      )
                      probs <- predict(mod, test_data, type = "raw")
                      if (!is.matrix(probs)) probs <- as.matrix(probs)
                      colnames(probs) <- levels(train_data[[target]])
                      probs
                    },
                    "rpart" = {
                      mod <- rpart::rpart(
                        formula(paste(target, "~ .")),
                        data = train_data,
                        cp = 0.01,
                        minsplit = 20,
                        maxdepth = 30
                      )
                      predict(mod, test_data, type = "prob")
                    },
                    "knn" = {
                      train_kknn <- cbind(Class = train_data[[target]], 
                                          train_data[, !(names(train_data) %in% target)])
                      test_kknn <- test_data[, !(names(test_data) %in% target)]
                      mod <- kknn::kknn(
                        Class ~ .,
                        train = train_kknn,
                        test = test_kknn,
                        k = 5,
                        kernel = "optimal"
                      )
                      mod$prob
                    },
                    "lda" = {
                      mod <- MASS::lda(
                        formula(paste(target, "~ .")),
                        data = train_data,
                        method = "moment"
                      )
                      predict(mod, test_data)$posterior
                    },
                    stop(paste("Модель", model_id, "не поддерживается в стекинге"))
                  )
                }
                
                # ---- Генерация мета-признаков через 5-кратную кросс-валидацию ----
                set.seed(111)
                folds <- caret::createFolds(y_train, k = 5, list = TRUE)
                meta_train_list <- list()
                meta_test_list <- list()
                
                for (fold in 1:5) {
                  tr_idx <- unlist(folds[-fold])
                  val_idx <- folds[[fold]]
                  tr <- train_data[tr_idx, ]
                  val <- train_data[val_idx, ]
                  
                  val_probs_list <- list()
                  test_probs_list <- list()
                  
                  for (bm in base_models) {
                    probs_val <- train_and_predict(bm, tr, val, target)
                    probs_test <- train_and_predict(bm, tr, test_data, target)
                    
                    for (j in 1:ncol(probs_val)) {
                      colname <- paste0(bm, "_", colnames(probs_val)[j])
                      val_probs_list[[colname]] <- probs_val[, j]
                      test_probs_list[[colname]] <- probs_test[, j]
                    }
                  }
                  
                  meta_fold <- as.data.frame(val_probs_list)
                  meta_fold$Class <- val[[target]]
                  meta_train_list[[fold]] <- meta_fold
                  meta_test_list[[fold]] <- as.data.frame(test_probs_list)
                }
                
                meta_train_all <- do.call(rbind, meta_train_list)
                meta_test_all <- Reduce("+", meta_test_list) / length(meta_test_list)
                
                # ---- Обучение метамодели ----
                if (meta_model == "glm") {
                  if (length(unique(meta_train_all$Class)) == 2) {
                    stack_mod <- glm(Class ~ ., data = meta_train_all, family = binomial(link = "logit"))
                    probs_positive <- predict(stack_mod, newdata = meta_test_all, type = "response")
                    probs <- cbind(1 - probs_positive, probs_positive)
                  } else {
                    stack_mod <- nnet::multinom(Class ~ ., data = meta_train_all, trace = FALSE, maxit = 500)
                    probs <- predict(stack_mod, newdata = meta_test_all, type = "probs")
                  }
                  colnames(probs) <- levels(y_train)
                } else if (meta_model == "rf") {
                  stack_mod <- randomForest::randomForest(Class ~ ., data = meta_train_all, ntree = 500)
                  probs <- predict(stack_mod, newdata = meta_test_all, type = "prob")
                } else if (meta_model == "et") {
                  stack_mod <- ranger::ranger(
                    dependent.variable.name = "Class",
                    data = meta_train_all,
                    num.trees = 500,
                    mtry = floor(sqrt(ncol(meta_train_all) - 1)),
                    min.node.size = 5,
                    splitrule = "extratrees",
                    replace = FALSE,
                    sample.fraction = 1,
                    probability = TRUE,
                    seed = 123
                  )
                  probs <- predict(stack_mod, data = meta_test_all)$predictions
                  colnames(probs) <- levels(y_train)
                } else if (meta_model == "xgb") {
                  to_numeric_meta <- function(df) {
                    as.data.frame(lapply(df, function(col) {
                      if (is.character(col)) as.numeric(as.factor(col)) else as.numeric(col)
                    }))
                  }
                  meta_train_num <- to_numeric_meta(meta_train_all[, !(names(meta_train_all) %in% "Class")])
                  meta_test_num <- to_numeric_meta(meta_test_all)
                  meta_train_num <- meta_train_num[, colSums(is.na(meta_train_num)) == 0, drop = FALSE]
                  meta_test_num <- meta_test_num[, colSums(is.na(meta_test_num)) == 0, drop = FALSE]
                  
                  is_binary_meta <- length(unique(meta_train_all$Class)) == 2
                  dtrain <- xgb.DMatrix(as.matrix(meta_train_num), 
                                        label = as.numeric(meta_train_all$Class) - 1)
                  dtest <- xgb.DMatrix(as.matrix(meta_test_num))
                  xgb_params <- list(
                    objective = if (is_binary_meta) "binary:logistic" else "multi:softprob",
                    num_class = if (is_binary_meta) 1 else length(unique(meta_train_all$Class)),
                    eval_metric = if (is_binary_meta) "logloss" else "mlogloss",
                    max_depth = 6,
                    eta = 0.1,
                    subsample = 0.8,
                    colsample_bytree = 0.8,
                    min_child_weight = 1
                  )
                  if (!is_binary_meta) xgb_params$num_class <- length(unique(meta_train_all$Class))
                  stack_mod <- xgb.train(xgb_params, dtrain, nrounds = 100, verbose = 0)
                  probs <- predict(stack_mod, dtest, reshape = TRUE)
                  if (is_binary_meta) {
                    probs <- cbind(1 - probs, probs)
                  }
                  colnames(probs) <- levels(y_train)
                } else if (meta_model == "ada") {
                  is_binary_meta <- length(unique(meta_train_all$Class)) == 2
                  if (is_binary_meta) {
                    stack_mod <- ada::ada(Class ~ ., data = meta_train_all, iter = 100, loss = "exponential")
                    probs <- predict(stack_mod, newdata = meta_test_all, type = "prob")
                  } else {
                    stack_mod <- adabag::boosting(
                      Class ~ .,
                      data = meta_train_all,
                      mfinal = 100,
                      control = rpart.control(maxdepth = 2, minsplit = 10),
                      coeflearn = "Breiman"
                    )
                    pred <- predict(stack_mod, newdata = meta_test_all)
                    probs <- pred$prob
                  }
                  colnames(probs) <- levels(y_train)
                } else if (meta_model == "nb") {
                  stack_mod <- e1071::naiveBayes(Class ~ ., data = meta_train_all, usekernel = FALSE, fL = 0)
                  probs <- predict(stack_mod, newdata = meta_test_all, type = "raw")
                  if (!is.matrix(probs)) probs <- as.matrix(probs)
                  colnames(probs) <- levels(y_train)
                } else if (meta_model == "gbm") {
                  stack_mod <- gbm::gbm(
                    Class ~ .,
                    data = meta_train_all,
                    n.trees = 500,
                    interaction.depth = 3,
                    shrinkage = 0.01,
                    bag.fraction = 0.5,
                    distribution = "multinomial",
                    verbose = FALSE
                  )
                  p_raw <- predict(stack_mod, meta_test_all, n.trees = 500, type = "response")
                  if (is.array(p_raw) && length(dim(p_raw)) == 3) {
                    probs <- as.matrix(p_raw[,,1])
                  } else {
                    probs <- as.matrix(p_raw)
                  }
                  colnames(probs) <- levels(y_train)
                } else if (meta_model == "rpart") {
                  stack_mod <- rpart::rpart(Class ~ ., data = meta_train_all, cp = 0.01, minsplit = 20, maxdepth = 30)
                  probs <- predict(stack_mod, newdata = meta_test_all, type = "prob")
                } else if (meta_model == "knn") {
                  train_kknn <- cbind(Class = meta_train_all$Class, 
                                      meta_train_all[, !(names(meta_train_all) %in% "Class")])
                  test_kknn <- meta_test_all
                  stack_mod <- kknn::kknn(
                    Class ~ .,
                    train = train_kknn,
                    test = test_kknn,
                    k = 5,
                    kernel = "optimal"
                  )
                  probs <- stack_mod$prob
                  colnames(probs) <- levels(y_train)
                } else if (meta_model == "lda") {
                  stack_mod <- MASS::lda(Class ~ ., data = meta_train_all, method = "moment")
                  probs <- predict(stack_mod, newdata = meta_test_all)$posterior
                } else {
                  stop("Неподдерживаемая метамодель")
                }
                
                metrics <- calc_metrics(probs, y_test)
                model_path <- save_model(stack_mod, user_id, model_id)
                trained_models_meta[[model_id]] <- list(
                  path = model_path,
                  metrics = metrics,
                  params = params,
                  class = "stacking"
                )
                rm(stack_mod); gc()
                data.frame(Model = paste("Stacking", meta_model), t(metrics), stringsAsFactors = FALSE)
              },
              # --- Логистическая регрессия ---
              "glm" = {
                if (length(unique(y_train)) == 2) {
                  mod <- glm(
                    formula(paste(target, "~ .")),
                    data = train_data,
                    family = binomial(link = "logit")
                  )
                  probs_positive <- predict(mod, X_test, type = "response")
                  probs <- cbind(1 - probs_positive, probs_positive)
                  colnames(probs) <- levels(y_train)
                  metrics <- calc_metrics(probs, y_test)
                  model_path <- save_model(mod, user_id, model_id)
                  trained_models_meta[[model_id]] <- list(
                    path = model_path,
                    metrics = metrics,
                    params = params,
                    class = "glm"
                  )
                  rm(mod); gc()
                  data.frame(Model = "LogReg", t(metrics), stringsAsFactors = FALSE)
                } else {
                  mod <- nnet::multinom(
                    formula(paste(target, "~ .")),
                    data = train_data,
                    maxit = params$maxit,
                    trace = FALSE
                  )
                  probs <- predict(mod, X_test, type = "probs")
                  metrics <- calc_metrics(probs, y_test)
                  model_path <- save_model(mod, user_id, model_id)
                  trained_models_meta[[model_id]] <- list(
                    path = model_path,
                    metrics = metrics,
                    params = params,
                    class = "multinom"
                  )
                  rm(mod); gc()
                  data.frame(Model = "LogReg", t(metrics), stringsAsFactors = FALSE)
                }
              },
              # --- k-NN ---
              "knn" = {
                train_kknn <- cbind(Class = y_train, X_train)
                test_kknn <- X_test
                mod <- kknn::kknn(
                  Class ~ .,
                  train = train_kknn,
                  test = test_kknn,
                  k = params$k,
                  kernel = params$kernel
                )
                probs <- mod$prob
                colnames(probs) <- levels(y_train)
                metrics <- calc_metrics(probs, y_test)
                model_path <- save_model(mod, user_id, model_id)
                trained_models_meta[[model_id]] <- list(
                  path = model_path,
                  metrics = metrics,
                  params = params,
                  class = "kknn"
                )
                rm(mod); gc()
                data.frame(Model = "kNN", t(metrics), stringsAsFactors = FALSE)
              },
              # --- Дерево решений ---
              "rpart" = {
                mod <- rpart::rpart(
                  formula(paste(target, "~ .")),
                  data = train_data,
                  cp = params$cp,
                  minsplit = params$minsplit,
                  maxdepth = params$maxdepth
                )
                probs <- predict(mod, X_test, type = "prob")
                metrics <- calc_metrics(probs, y_test)
                model_path <- save_model(mod, user_id, model_id)
                trained_models_meta[[model_id]] <- list(
                  path = model_path,
                  metrics = metrics,
                  params = params,
                  class = "rpart"
                )
                rm(mod); gc()
                data.frame(Model = "rpart", t(metrics), stringsAsFactors = FALSE)
              },
              # --- Наивный Байес ---
              "nb" = {
                mod <- e1071::naiveBayes(
                  formula(paste(target, "~ .")),
                  data = train_data,
                  laplace = params$fL
                )
                probs <- predict(mod, X_test, type = "raw")
                if (is.matrix(probs)) {
                  colnames(probs) <- levels(y_train)
                } else {
                  probs <- as.matrix(probs)
                  colnames(probs) <- levels(y_train)
                }
                metrics <- calc_metrics(probs, y_test)
                model_path <- save_model(mod, user_id, model_id)
                trained_models_meta[[model_id]] <- list(
                  path = model_path,
                  metrics = metrics,
                  params = params,
                  class = "naiveBayes"
                )
                rm(mod); gc()
                data.frame(Model = "NaiveBayes", t(metrics), stringsAsFactors = FALSE)
              },
              # --- ЛДА ---
              "lda" = {
                mod <- MASS::lda(
                  formula(paste(target, "~ .")),
                  data = train_data
                )
                probs <- predict(mod, X_test)$posterior
                metrics <- calc_metrics(probs, y_test)
                model_path <- save_model(mod, user_id, model_id)
                trained_models_meta[[model_id]] <- list(
                  path = model_path,
                  metrics = metrics,
                  params = params,
                  class = "lda"
                )
                rm(mod); gc()
                data.frame(Model = "LDA", t(metrics), stringsAsFactors = FALSE)
              }
            )
          }, error = function(e) {
            showNotification(paste("Ошибка в модели", model_id, ":", e$message), type = "error")
            return(NULL)
          })
          
          if (!is.null(result)) {
            results_all[[model_id]] <- result
          }
        }
      })
      
      # ---- Формирование итоговой таблицы и сохранение ----
      if (length(results_all) > 0) {
        final_results <- do.call(rbind, results_all)
        rownames(final_results) <- NULL
        params_str <- sapply(selected_models, function(m) {
          if (m == "stack") {
            p <- params_list$params$stack
            paste0("base_models = ", paste(p$base_models, collapse = ", "),
                   "; meta_model = ", p$meta_model)
          } else {
            p <- params_list$params[[m]]
            paste(names(p), unlist(p), sep="=", collapse="; ")
          }
        })
        final_results$Params <- params_str[1:nrow(final_results)]
        
        session_data$trained_models <- trained_models_meta
        session_data$training_results <- final_results
        
        save_user_data(user_id, session_data)
        showNotification("Обучение завершено! Модели сохранены на диск.", type = "message")
      } else {
        showNotification("Не удалось обучить ни одну модель", type = "warning")
        session_data$trained_models <- list()
        session_data$training_results <- NULL
      }
      
      rm(df, train_data, test_data, X_train, y_train, X_test, y_test); gc()
    })
    
    # ---- Очистка результатов ----
    observeEvent(input$clear_btn, {
      if (!is.null(session_data$trained_models)) {
        for (meta in session_data$trained_models) {
          if (file.exists(meta$path)) file.remove(meta$path)
        }
      }
      session_data$trained_models <- list()
      session_data$training_results <- NULL
      showNotification("Результаты и модели очищены", type = "message")
    })
    
    # ---- Отображение результатов ----
    output$results_output <- renderUI({
      res <- session_data$training_results
      if (is.null(res)) {
        return(div(class = "alert alert-info", "Результаты обучения появятся здесь после нажатия кнопки."))
      }
      tagList(
        h4("Результаты обучения моделей"),
        DT::renderDT({
          DT::datatable(res, options = list(pageLength = 10, scrollX = TRUE, ordering = TRUE),
                        rownames = FALSE)
        })
      )
    })
    
  })
}