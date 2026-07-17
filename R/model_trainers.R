# R/model_trainers.R
# Единый движок обучения и предсказания для всех моделей
# Используется modelTrainingModule и modelPredictionModule

# =============================================================================
# Расчёт метрик классификации
# =============================================================================

calc_metrics <- function(probs, true) {
  classes <- levels(true)
  pred <- classes[apply(probs, 1, which.max)]
  acc <- mean(pred == true)
  
  prec <- recall <- f1 <- numeric(length(classes))
  for (i in seq_along(classes)) {
    tp <- sum(pred == classes[i] & true == classes[i])
    fp <- sum(pred == classes[i] & true != classes[i])
    fn <- sum(pred != classes[i] & true == classes[i])
    prec[i] <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
    recall[i] <- ifelse(tp + fn == 0, 0, tp / (tp + fn))
    f1[i] <- ifelse(prec[i] + recall[i] == 0, 0, 2 * prec[i] * recall[i] / (prec[i] + recall[i]))
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
  
  c(Accuracy = acc, Precision = macro_prec, Recall = macro_rec, F1 = macro_f1, AUC = macro_auc)
}

# =============================================================================
# Обучение одной модели
# =============================================================================

train_single_model <- function(model_id, params, train_data, test_data, target) {
  X_train <- train_data[, !(names(train_data) %in% target), drop = FALSE]
  y_train <- train_data[[target]]
  X_test <- test_data[, !(names(test_data) %in% target), drop = FALSE]
  y_test <- test_data[[target]]
  
  switch(
    model_id,
    
    # --- Случайный лес ---
    "rf" = {
      mod <- randomForest::randomForest(
        x = X_train, y = y_train,
        ntree = params$ntree %||% 500,
        mtry = params$mtry %||% floor(sqrt(ncol(X_train))),
        nodesize = params$nodesize %||% 5,
        importance = FALSE
      )
      probs <- predict(mod, X_test, type = "prob")
      metrics <- calc_metrics(probs, y_test)
      list(model = mod, metrics = metrics, probs = probs, class = "randomForest",
           label = "RandomForest")
    },
    
    # --- Extra Trees ---
    "et" = {
      mod <- ranger::ranger(
        dependent.variable.name = target,
        data = train_data,
        num.trees = params$ntree %||% 500,
        mtry = params$mtry %||% floor(sqrt(ncol(train_data) - 1)),
        min.node.size = params$nodesize %||% 5,
        splitrule = "extratrees",
        replace = FALSE,
        sample.fraction = 1,
        probability = TRUE,
        seed = 123
      )
      probs <- predict(mod, data = X_test)$predictions
      colnames(probs) <- levels(y_train)
      metrics <- calc_metrics(probs, y_test)
      list(model = mod, metrics = metrics, probs = probs, class = "ranger",
           label = "ExtraTrees")
    },
    
    # --- GBM ---
    "gbm" = {
      mod <- gbm::gbm(
        stats::formula(paste(target, "~ .")),
        data = train_data,
        n.trees = params$n.trees %||% 500,
        interaction.depth = params$interaction.depth %||% 3,
        shrinkage = params$shrinkage %||% 0.1,
        bag.fraction = params$bag.fraction %||% 0.5,
        distribution = "multinomial",
        verbose = FALSE
      )
      p_raw <- predict(mod, X_test, n.trees = params$n.trees %||% 500, type = "response")
      if (is.array(p_raw) && length(dim(p_raw)) == 3) {
        probs <- as.matrix(p_raw[, , 1])
      } else {
        probs <- as.matrix(p_raw)
      }
      colnames(probs) <- levels(y_train)
      metrics <- calc_metrics(probs, y_test)
      list(model = mod, metrics = metrics, probs = probs, class = "gbm",
           label = "GBM")
    },
    
    # --- XGBoost ---
    "xgb" = {
      X_train_num <- df_to_numeric_matrix(X_train)
      X_test_num <- df_to_numeric_matrix(X_test)
      if (any(is.na(X_train_num)) || any(is.na(X_test_num))) {
        stop("В данных есть пропуски (NA). XGBoost не может их обработать.")
      }
      y_train_xgb <- as.numeric(y_train) - 1
      y_test_xgb <- as.numeric(y_test) - 1
      
      dtrain <- xgboost::xgb.DMatrix(X_train_num, label = y_train_xgb)
      dtest <- xgboost::xgb.DMatrix(X_test_num, label = y_test_xgb)
      
      xgb_params <- list(
        objective = "multi:softprob",
        num_class = length(unique(y_train)),
        eval_metric = "mlogloss",
        max_depth = params$max_depth %||% 6,
        eta = params$eta %||% 0.3,
        subsample = params$subsample %||% 1,
        colsample_bytree = params$colsample_bytree %||% 1,
        min_child_weight = 1
      )
      mod <- xgboost::xgb.train(xgb_params, dtrain, nrounds = params$nrounds %||% 100, verbose = 0)
      probs <- predict(mod, dtest, reshape = TRUE)
      colnames(probs) <- levels(y_train)
      metrics <- calc_metrics(probs, y_test)
      list(model = mod, metrics = metrics, probs = probs, class = "xgboost",
           label = "XGBoost")
    },
    
    # --- AdaBoost ---
    "ada" = {
      if (is_binary_classification(y_train)) {
        mod <- ada::ada(
          stats::formula(paste(target, "~ .")),
          data = train_data,
          iter = params$mfinal %||% 100,
          loss = "exponential"
        )
        probs <- predict(mod, newdata = test_data, type = "prob")
      } else {
        mod <- adabag::boosting(
          stats::formula(paste(target, "~ .")),
          data = train_data,
          mfinal = params$mfinal %||% 100,
          control = rpart::rpart.control(maxdepth = params$maxdepth %||% 2, minsplit = 10),
          coeflearn = params$coeflearn %||% "Breiman"
        )
        pred <- predict(mod, newdata = test_data)
        probs <- pred$prob
      }
      colnames(probs) <- levels(y_train)
      metrics <- calc_metrics(probs, y_test)
      list(model = mod, metrics = metrics, probs = probs, class = "ada",
           label = "AdaBoost")
    },
    
    # --- Логистическая регрессия ---
    "glm" = {
      if (is_binary_classification(y_train)) {
        mod <- glm(
          stats::formula(paste(target, "~ .")),
          data = train_data,
          family = binomial(link = "logit")
        )
        probs_positive <- predict(mod, X_test, type = "response")
        probs <- cbind(1 - probs_positive, probs_positive)
      } else {
        mod <- nnet::multinom(
          stats::formula(paste(target, "~ .")),
          data = train_data,
          maxit = params$maxit %||% 100,
          trace = FALSE
        )
        probs <- predict(mod, X_test, type = "probs")
      }
      colnames(probs) <- levels(y_train)
      metrics <- calc_metrics(probs, y_test)
      list(model = mod, metrics = metrics, probs = probs, 
           class = if (is_binary_classification(y_train)) "glm" else "multinom",
           label = "LogReg")
    },
    
    # --- k-NN ---
    "knn" = {
      train_kknn <- cbind(Class = y_train, X_train)
      test_kknn <- X_test
      mod <- kknn::kknn(
        Class ~ .,
        train = train_kknn,
        test = test_kknn,
        k = params$k %||% 5,
        kernel = params$kernel %||% "optimal"
      )
      probs <- mod$prob
      colnames(probs) <- levels(y_train)
      metrics <- calc_metrics(probs, y_test)
      # Для kknn сохраняем обучающие данные
      mod$train <- train_kknn
      list(model = mod, metrics = metrics, probs = probs, class = "kknn",
           label = "kNN")
    },
    
    # --- Дерево решений ---
    "rpart" = {
      mod <- rpart::rpart(
        stats::formula(paste(target, "~ .")),
        data = train_data,
        cp = params$cp %||% 0.01,
        minsplit = params$minsplit %||% 20,
        maxdepth = params$maxdepth %||% 30
      )
      probs <- predict(mod, X_test, type = "prob")
      metrics <- calc_metrics(probs, y_test)
      list(model = mod, metrics = metrics, probs = probs, class = "rpart",
           label = "rpart")
    },
    
    # --- Наивный Байес ---
    "nb" = {
      mod <- e1071::naiveBayes(
        stats::formula(paste(target, "~ .")),
        data = train_data,
        laplace = params$fL %||% 0
      )
      probs <- predict(mod, X_test, type = "raw")
      if (!is.matrix(probs)) probs <- as.matrix(probs)
      colnames(probs) <- levels(y_train)
      metrics <- calc_metrics(probs, y_test)
      list(model = mod, metrics = metrics, probs = probs, class = "naiveBayes",
           label = "NaiveBayes")
    },
    
    # --- ЛДА ---
    "lda" = {
      mod <- MASS::lda(
        stats::formula(paste(target, "~ .")),
        data = train_data
      )
      probs <- predict(mod, X_test)$posterior
      metrics <- calc_metrics(probs, y_test)
      list(model = mod, metrics = metrics, probs = probs, class = "lda",
           label = "LDA")
    },
    
    stop(paste("Неизвестная модель:", model_id))
  )
}

# =============================================================================
# Предсказание одной обученной моделью
# =============================================================================

predict_with_model <- function(mod, new_data, model_id, model_params = list()) {
  switch(
    model_id,
    "rf" = {
      pred <- predict(mod, newdata = new_data)
      list(class = as.character(pred))
    },
    "et" = {
      pred <- predict(mod, data = new_data)
      classes <- colnames(pred$predictions)
      class_idx <- apply(pred$predictions, 1, which.max)
      list(class = classes[class_idx], prob = pred$predictions)
    },
    "gbm" = {
      n_trees <- model_params$n.trees %||% 100
      pred <- predict(mod, newdata = new_data, n.trees = n_trees, type = "response")
      if (is.array(pred) && length(dim(pred)) == 3) {
        probs <- pred[, , 1]
        class_idx <- apply(probs, 1, which.max)
        classes <- colnames(probs)
        list(class = classes[class_idx], prob = probs)
      } else {
        list(class = as.character(pred))
      }
    },
    "xgb" = {
      new_data_num <- df_to_numeric_matrix(new_data)
      dtest <- xgboost::xgb.DMatrix(new_data_num)
      probs <- predict(mod, dtest, reshape = TRUE)
      if (is.null(dim(probs))) {
        # Бинарный случай
        list(class = ifelse(probs > 0.5, colnames(mod$params)[2], colnames(mod$params)[1]),
             prob = probs)
      } else {
        class_idx <- apply(probs, 1, which.max)
        classes <- colnames(probs)
        list(class = classes[class_idx], prob = probs)
      }
    },
    "ada" = {
      if (inherits(mod, "ada")) {
        pred <- predict(mod, newdata = new_data, type = "prob")
        list(class = colnames(pred)[apply(pred, 1, which.max)], prob = pred)
      } else {
        pred <- predict(mod, newdata = new_data)
        list(class = as.character(pred$class), prob = pred$prob)
      }
    },
    "glm" = {
      if (inherits(mod, "glm")) {
        prob_pos <- predict(mod, newdata = new_data, type = "response")
        list(class = ifelse(prob_pos > 0.5, "1", "0"), prob = prob_pos)
      } else {
        pred <- predict(mod, newdata = new_data, type = "probs")
        class_idx <- apply(pred, 1, which.max)
        list(class = colnames(pred)[class_idx], prob = pred)
      }
    },
    "knn" = {
      pred <- kknn::kknn(
        Class ~ .,
        train = mod$train,
        test = new_data,
        k = mod$k,
        kernel = mod$kernel
      )
      list(class = as.character(pred$fitted.values), prob = pred$prob)
    },
    "rpart" = {
      pred <- predict(mod, newdata = new_data, type = "prob")
      class_idx <- apply(pred, 1, which.max)
      list(class = colnames(pred)[class_idx], prob = pred)
    },
    "nb" = {
      pred <- predict(mod, newdata = new_data, type = "raw")
      if (!is.matrix(pred)) pred <- as.matrix(pred)
      class_idx <- apply(pred, 1, which.max)
      list(class = colnames(pred)[class_idx], prob = pred)
    },
    "lda" = {
      pred <- predict(mod, newdata = new_data)
      list(class = as.character(pred$class), prob = pred$posterior)
    },
    "stack" = {
      # Для стекинга используется predict по умолчанию
      pred <- predict(mod, newdata = new_data)
      if (is.factor(pred)) {
        list(class = as.character(pred))
      } else if (is.matrix(pred) || is.data.frame(pred)) {
        class_idx <- apply(pred, 1, which.max)
        list(class = colnames(pred)[class_idx], prob = pred)
      } else {
        list(class = as.character(pred))
      }
    },
    stop(paste("Неизвестная модель для предсказания:", model_id))
  )
}