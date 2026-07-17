# R/stacking_utils.R
# Логика стекинга: генерация мета-признаков через кросс-валидацию
# Использует train_single_model() из model_trainers.R

# =============================================================================
# Вспомогательная: обучение базовой модели и получение вероятностей
# =============================================================================

train_base_and_predict <- function(model_id, params, train_data, test_data, target) {
  result <- train_single_model(model_id, params, train_data, test_data, target)
  result$probs
}

# =============================================================================
# Генерация мета-признаков через 5-кратную кросс-валидацию
# =============================================================================

generate_meta_features <- function(base_models, base_params, train_data, test_data, target, k = 5) {
  set.seed(111)
  y_train <- train_data[[target]]
  folds <- caret::createFolds(y_train, k = k, list = TRUE)
  
  meta_train_list <- list()
  meta_test_list <- list()
  
  for (fold in 1:k) {
    tr_idx <- unlist(folds[-fold])
    val_idx <- folds[[fold]]
    tr <- train_data[tr_idx, ]
    val <- train_data[val_idx, ]
    
    val_probs_list <- list()
    test_probs_list <- list()
    
    for (bm in base_models) {
      bm_params <- base_params[[bm]] %||% list()
      probs_val <- tryCatch({
        train_base_and_predict(bm, bm_params, tr, val, target)
      }, error = function(e) {
        warning(paste("Ошибка в базовой модели", bm, "на фолде", fold, ":", e$message))
        NULL
      })
      
      probs_test <- tryCatch({
        train_base_and_predict(bm, bm_params, tr, test_data, target)
      }, error = function(e) {
        warning(paste("Ошибка в базовой модели", bm, "на тесте:", e$message))
        NULL
      })
      
      if (!is.null(probs_val) && !is.null(probs_test)) {
        for (j in 1:ncol(probs_val)) {
          colname <- paste0(bm, "_", colnames(probs_val)[j])
          val_probs_list[[colname]] <- probs_val[, j]
          test_probs_list[[colname]] <- probs_test[, j]
        }
      }
    }
    
    if (length(val_probs_list) == 0) {
      stop("Ни одна базовая модель не сработала в стекинге")
    }
    
    meta_fold <- as.data.frame(val_probs_list)
    meta_fold$Class <- val[[target]]
    meta_train_list[[fold]] <- meta_fold
    meta_test_list[[fold]] <- as.data.frame(test_probs_list)
  }
  
  meta_train_all <- do.call(rbind, meta_train_list)
  meta_test_all <- Reduce("+", meta_test_list) / length(meta_test_list)
  
  list(train = meta_train_all, test = meta_test_all)
}

# =============================================================================
# Обучение метамодели
# =============================================================================

train_meta_model <- function(meta_train, meta_test, meta_model_id, y_train_levels, meta_params = list()) {
  is_binary <- length(unique(meta_train$Class)) == 2
  
  switch(
    meta_model_id,
    
    "glm" = {
      if (is_binary) {
        mod <- glm(Class ~ ., data = meta_train, family = binomial(link = "logit"))
        probs_positive <- predict(mod, newdata = meta_test, type = "response")
        probs <- cbind(1 - probs_positive, probs_positive)
      } else {
        mod <- nnet::multinom(Class ~ ., data = meta_train, trace = FALSE, maxit = meta_params$maxit %||% 500)
        probs <- predict(mod, newdata = meta_test, type = "probs")
      }
      colnames(probs) <- y_train_levels
      list(model = mod, probs = probs)
    },
    
    "rf" = {
      mod <- randomForest::randomForest(
        Class ~ ., data = meta_train,
        ntree = meta_params$ntree %||% 500,
        mtry = meta_params$mtry %||% floor(sqrt(ncol(meta_train) - 1)),
        nodesize = meta_params$nodesize %||% 5
      )
      probs <- predict(mod, newdata = meta_test, type = "prob")
      colnames(probs) <- y_train_levels
      list(model = mod, probs = probs)
    },
    
    "et" = {
      mod <- ranger::ranger(
        dependent.variable.name = "Class",
        data = meta_train,
        num.trees = meta_params$ntree %||% 500,
        mtry = meta_params$mtry %||% floor(sqrt(ncol(meta_train) - 1)),
        min.node.size = meta_params$nodesize %||% 5,
        splitrule = "extratrees",
        replace = FALSE,
        sample.fraction = 1,
        probability = TRUE,
        seed = 123
      )
      probs <- predict(mod, data = meta_test)$predictions
      colnames(probs) <- y_train_levels
      list(model = mod, probs = probs)
    },
    
    "xgb" = {
      meta_train_num <- df_to_numeric_matrix(meta_train[, !(names(meta_train) %in% "Class")])
      meta_test_num <- df_to_numeric_matrix(meta_test)
      meta_train_num <- meta_train_num[, colSums(is.na(meta_train_num)) == 0, drop = FALSE]
      meta_test_num <- meta_test_num[, colSums(is.na(meta_test_num)) == 0, drop = FALSE]
      
      dtrain <- xgboost::xgb.DMatrix(meta_train_num, label = as.numeric(meta_train$Class) - 1)
      dtest <- xgboost::xgb.DMatrix(meta_test_num)
      
      xgb_params <- list(
        objective = if (is_binary) "binary:logistic" else "multi:softprob",
        num_class = if (is_binary) 1 else length(unique(meta_train$Class)),
        eval_metric = if (is_binary) "logloss" else "mlogloss",
        max_depth = meta_params$max_depth %||% 6,
        eta = meta_params$eta %||% 0.1,
        subsample = meta_params$subsample %||% 0.8,
        colsample_bytree = meta_params$colsample_bytree %||% 0.8,
        min_child_weight = 1
      )
      if (!is_binary) xgb_params$num_class <- length(unique(meta_train$Class))
      
      mod <- xgboost::xgb.train(xgb_params, dtrain, nrounds = meta_params$nrounds %||% 100, verbose = 0)
      probs <- predict(mod, dtest, reshape = TRUE)
      if (is_binary) {
        probs <- cbind(1 - probs, probs)
      }
      colnames(probs) <- y_train_levels
      list(model = mod, probs = probs)
    },
    
    "ada" = {
      if (is_binary) {
        mod <- ada::ada(
          Class ~ ., data = meta_train,
          iter = meta_params$mfinal %||% 100,
          loss = "exponential"
        )
        probs <- predict(mod, newdata = meta_test, type = "prob")
      } else {
        mod <- adabag::boosting(
          Class ~ .,
          data = meta_train,
          mfinal = meta_params$mfinal %||% 100,
          control = rpart::rpart.control(maxdepth = meta_params$maxdepth %||% 2, minsplit = 10),
          coeflearn = meta_params$coeflearn %||% "Breiman"
        )
        pred <- predict(mod, newdata = meta_test)
        probs <- pred$prob
      }
      colnames(probs) <- y_train_levels
      list(model = mod, probs = probs)
    },
    
    "nb" = {
      mod <- e1071::naiveBayes(Class ~ ., data = meta_train, usekernel = FALSE, fL = meta_params$fL %||% 0)
      probs <- predict(mod, newdata = meta_test, type = "raw")
      if (!is.matrix(probs)) probs <- as.matrix(probs)
      colnames(probs) <- y_train_levels
      list(model = mod, probs = probs)
    },
    
    "gbm" = {
      mod <- gbm::gbm(
        Class ~ .,
        data = meta_train,
        n.trees = meta_params$n.trees %||% 500,
        interaction.depth = meta_params$interaction.depth %||% 3,
        shrinkage = meta_params$shrinkage %||% 0.01,
        bag.fraction = meta_params$bag.fraction %||% 0.5,
        distribution = "multinomial",
        verbose = FALSE
      )
      p_raw <- predict(mod, meta_test, n.trees = meta_params$n.trees %||% 500, type = "response")
      if (is.array(p_raw) && length(dim(p_raw)) == 3) {
        probs <- as.matrix(p_raw[, , 1])
      } else {
        probs <- as.matrix(p_raw)
      }
      colnames(probs) <- y_train_levels
      list(model = mod, probs = probs)
    },
    
    "rpart" = {
      mod <- rpart::rpart(
        Class ~ ., data = meta_train,
        cp = meta_params$cp %||% 0.01,
        minsplit = meta_params$minsplit %||% 20,
        maxdepth = meta_params$maxdepth %||% 30
      )
      probs <- predict(mod, newdata = meta_test, type = "prob")
      colnames(probs) <- y_train_levels
      list(model = mod, probs = probs)
    },
    
    "knn" = {
      train_kknn <- cbind(Class = meta_train$Class, 
                          meta_train[, !(names(meta_train) %in% "Class")])
      test_kknn <- meta_test
      mod <- kknn::kknn(
        Class ~ .,
        train = train_kknn,
        test = test_kknn,
        k = meta_params$k %||% 5,
        kernel = meta_params$kernel %||% "optimal"
      )
      probs <- mod$prob
      colnames(probs) <- y_train_levels
      list(model = mod, probs = probs)
    },
    
    "lda" = {
      mod <- MASS::lda(Class ~ ., data = meta_train, method = "moment")
      probs <- predict(mod, newdata = meta_test)$posterior
      colnames(probs) <- y_train_levels
      list(model = mod, probs = probs)
    },
    
    stop(paste("Неподдерживаемая метамодель:", meta_model_id))
  )
}

# =============================================================================
# Полный пайплайн стекинга
# =============================================================================

train_stacking <- function(params, train_data, test_data, target) {
  base_models <- params$base_models
  meta_model_id <- params$meta_model
  base_params <- params$base_params %||% list()
  meta_params <- params$meta_params %||% list()
  
  if (length(base_models) == 0) {
    stop("Не выбрано ни одной базовой модели для стекинга")
  }
  
  # Генерация мета-признаков
  meta <- generate_meta_features(base_models, base_params, train_data, test_data, target)
  
  # Обучение метамодели с пользовательскими параметрами
  y_train_levels <- levels(train_data[[target]])
  result <- train_meta_model(meta$train, meta$test, meta_model_id, y_train_levels, meta_params)
  
  # Расчёт метрик
  y_test <- test_data[[target]]
  metrics <- calc_metrics(result$probs, y_test)
  
  list(
    model = result$model,
    metrics = metrics,
    probs = result$probs,
    class = "stacking",
    label = paste("Stacking", meta_model_id)
  )
}