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
      raw_pred <- predict(mod, dtest)
      n_classes <- length(levels(y_train))
      if (n_classes == 2) {
        probs <- matrix(c(1 - raw_pred, raw_pred), nrow = 1)
      } else {
        probs <- matrix(as.numeric(raw_pred), nrow = nrow(X_test), byrow = TRUE)
      }
      colnames(probs) <- levels(y_train)
      metrics <- calc_metrics(probs, y_test)
      list(model = mod, metrics = metrics, probs = probs, class = "xgboost",
           label = "XGBoost")
    },
    
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
      # Сохраняем обучающие данные и параметры для предсказания
      mod$train_data <- train_kknn
      mod$k_value <- params$k %||% 5
      mod$kernel_value <- params$kernel %||% "optimal"
      list(model = mod, metrics = metrics, probs = probs, class = "kknn",
           label = "kNN")
    },
    
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
# Всегда возвращает list(class = "имя_класса", prob = матрица_вероятностей)

predict_with_model <- function(mod, new_data, model_id, model_params = list(), class_levels = NULL) {
  switch(
    model_id,
    
    "rf" = {
      pred <- predict(mod, newdata = new_data, type = "prob")
      classes <- colnames(pred)
      class_idx <- apply(pred, 1, which.max)
      list(class = classes[class_idx], prob = pred)
    },
    
    "et" = {
      pred <- predict(mod, data = new_data)
      probs <- pred$predictions
      classes <- colnames(probs)
      class_idx <- apply(probs, 1, which.max)
      list(class = classes[class_idx], prob = probs)
    },
    
    "gbm" = {
      n_trees <- model_params$n.trees %||% 100
      pred <- predict(mod, newdata = new_data, n.trees = n_trees, type = "response")
      if (is.array(pred) && length(dim(pred)) == 3) {
        # Для 1 строки: pred[1, , 1] — вектор вероятностей
        probs <- matrix(pred[1, , 1], nrow = 1)
        classes <- class_levels %||% paste0("Class", 1:ncol(probs))
        colnames(probs) <- classes
        class_idx <- which.max(probs[1, ])
        list(class = classes[class_idx], prob = probs)
      } else if (is.matrix(pred)) {
        probs <- pred
        classes <- colnames(probs)
        if (is.null(classes)) classes <- class_levels %||% paste0("Class", 1:ncol(probs))
        colnames(probs) <- classes
        class_idx <- apply(probs, 1, which.max)
        list(class = classes[class_idx], prob = probs)
      } else {
        # Вектор — бинарный случай
        classes <- class_levels %||% c("0", "1")
        probs <- matrix(pred, nrow = 1)
        colnames(probs) <- classes
        class_idx <- which.max(probs[1, ])
        list(class = classes[class_idx], prob = probs)
      }
    },
    
    "xgb" = {
      new_data_num <- df_to_numeric_matrix(new_data)
      dtest <- xgboost::xgb.DMatrix(new_data_num)
      raw_pred <- predict(mod, dtest)
      n_classes <- length(class_levels %||% c("0", "1"))
      if (n_classes == 2 && !is.null(dim(raw_pred))) {
        probs <- matrix(as.numeric(raw_pred), nrow = 1, byrow = TRUE)
        classes <- class_levels %||% paste0("Class", 1:ncol(probs))
        colnames(probs) <- classes
        class_idx <- which.max(probs[1, ])
        list(class = classes[class_idx], prob = probs)
      } else {
        classes <- class_levels %||% c("0", "1")
        probs_matrix <- if (is.null(dim(raw_pred))) {
          matrix(c(1 - raw_pred, raw_pred), nrow = 1)
        } else {
          matrix(as.numeric(raw_pred), nrow = 1, byrow = TRUE)
        }
        colnames(probs_matrix) <- classes
        class_idx <- ifelse(probs_matrix[1, 2] > 0.5, 2, 1)
        list(class = classes[class_idx], prob = probs_matrix)
      }
    },
    
    "ada" = {
      if (inherits(mod, "ada")) {
        pred <- predict(mod, newdata = new_data, type = "prob")
        classes <- colnames(pred)
        class_idx <- apply(pred, 1, which.max)
        list(class = classes[class_idx], prob = pred)
      } else {
        pred <- predict(mod, newdata = new_data)
        probs <- pred$prob
        classes <- colnames(probs)
        if (is.null(classes)) classes <- class_levels %||% paste0("Class", 1:ncol(probs))
        colnames(probs) <- classes
        class_idx <- apply(probs, 1, which.max)
        list(class = classes[class_idx], prob = probs)
      }
    },
    
    "glm" = {
      if (inherits(mod, "glm")) {
        prob_pos <- predict(mod, newdata = new_data, type = "response")
        classes <- class_levels %||% c("0", "1")
        probs <- matrix(c(1 - prob_pos, prob_pos), nrow = 1, dimnames = list(NULL, classes))
        class_idx <- ifelse(prob_pos > 0.5, 2, 1)
        list(class = classes[class_idx], prob = probs)
      } else {
        pred <- predict(mod, newdata = new_data, type = "probs")
        classes <- colnames(pred)
        if (is.null(classes)) classes <- class_levels %||% paste0("Class", 1:ncol(pred))
        if (!is.matrix(pred)) pred <- matrix(pred, nrow = 1)
        colnames(pred) <- classes
        class_idx <- apply(pred, 1, which.max)
        list(class = classes[class_idx], prob = pred)
      }
    },
    
    "knn" = {
      # Для kknn нужно передать обучающие данные и параметры
      train_data <- model_params$train_data
      k_val <- model_params$k %||% 5
      kernel_val <- model_params$kernel %||% "optimal"
      
      if (is.null(train_data)) {
        # Пробуем достать из объекта модели
        train_data <- mod$train_data
        k_val <- mod$k_value %||% k_val
        kernel_val <- mod$kernel_value %||% kernel_val
      }
      
      if (is.null(train_data)) {
        stop("Для kNN не сохранены обучающие данные. Переобучите модель.")
      }
      
      pred <- kknn::kknn(
        Class ~ .,
        train = train_data,
        test = new_data,
        k = k_val,
        kernel = kernel_val
      )
      probs <- pred$prob
      classes <- colnames(probs)
      if (is.null(classes)) classes <- class_levels %||% paste0("Class", 1:ncol(probs))
      colnames(probs) <- classes
      list(class = as.character(pred$fitted.values), prob = probs)
    },
    
    "rpart" = {
      pred <- predict(mod, newdata = new_data, type = "prob")
      classes <- colnames(pred)
      class_idx <- apply(pred, 1, which.max)
      list(class = classes[class_idx], prob = pred)
    },
    
    "nb" = {
      pred <- predict(mod, newdata = new_data, type = "raw")
      if (!is.matrix(pred)) pred <- as.matrix(pred)
      classes <- colnames(pred)
      if (is.null(classes)) classes <- class_levels %||% paste0("Class", 1:ncol(pred))
      colnames(pred) <- classes
      class_idx <- apply(pred, 1, which.max)
      list(class = classes[class_idx], prob = pred)
    },
    
    "lda" = {
      pred <- predict(mod, newdata = new_data)
      probs <- pred$posterior
      classes <- colnames(probs)
      if (is.null(classes)) classes <- class_levels %||% paste0("Class", 1:ncol(probs))
      colnames(probs) <- classes
      list(class = as.character(pred$class), prob = probs)
    },
    
    "stack" = {
      # Для стекинга model_params = stack$params
      meta_model_id <- model_params$meta_model %||% "glm"
      meta_params <- model_params$meta_params %||% list()
      
      # Проверяем, есть ли сохранённые обученные базовые модели
      if (!is.null(mod$stack_base_models_trained)) {
        meta_test <- predict_stack_base_models(
          mod$stack_base_models_trained,
          new_data,
          mod$stack_class_levels
        )
        pred_result <- predict_with_model(mod, meta_test, meta_model_id, meta_params, class_levels)
        return(pred_result)
      }
      
      # Иначе пытаемся предсказать напрямую
      pred <- predict(mod, newdata = new_data)
      if (is.factor(pred)) {
        classes <- levels(pred)
        probs <- matrix(0, nrow = 1, ncol = length(classes))
        colnames(probs) <- classes
        probs[1, as.character(pred)] <- 1
        list(class = as.character(pred), prob = probs)
      } else if (is.matrix(pred) || is.data.frame(pred)) {
        probs <- as.matrix(pred)
        classes <- colnames(probs)
        if (is.null(classes)) classes <- class_levels %||% paste0("Class", 1:ncol(probs))
        colnames(probs) <- classes
        class_idx <- apply(probs, 1, which.max)
        list(class = classes[class_idx], prob = probs)
      } else {
        classes <- class_levels %||% c("0", "1")
        probs <- matrix(c(1 - as.numeric(pred), as.numeric(pred)), nrow = 1)
        colnames(probs) <- classes
        class_idx <- which.max(probs[1, ])
        list(class = classes[class_idx], prob = probs)
      }
    },
    
    stop(paste("Неизвестная модель для предсказания:", model_id))
  )
}

# =============================================================================
# Предсказание для стекинга: генерация мета-признаков из обученных базовых моделей
# =============================================================================

predict_stack_base_models <- function(base_models_trained, new_data, class_levels) {
  probs_list <- list()
  
  for (bm in names(base_models_trained)) {
    mod <- base_models_trained[[bm]]
    
    if (inherits(mod, "randomForest")) {
      probs <- predict(mod, newdata = new_data, type = "prob")
    } else if (inherits(mod, "ranger")) {
      probs <- predict(mod, data = new_data)$predictions
    } else if (inherits(mod, "gbm")) {
      probs <- predict(mod, newdata = new_data, n.trees = mod$n.trees, type = "response")
      if (is.array(probs) && length(dim(probs)) == 3) {
        probs <- as.matrix(probs[, , 1])
      }
    } else if (inherits(mod, "xgb.Booster")) {
      dtest <- xgboost::xgb.DMatrix(df_to_numeric_matrix(new_data))
      raw_pred <- predict(mod, dtest)
      n_classes <- length(class_levels)
      if (n_classes == 2) {
        probs <- matrix(c(1 - raw_pred, raw_pred), nrow = 1)
      } else {
        probs <- matrix(as.numeric(raw_pred), nrow = 1, byrow = TRUE)
      }
    } else if (inherits(mod, "ada")) {
      probs <- predict(mod, newdata = new_data, type = "prob")
    } else if (inherits(mod, "boosting")) {
      probs <- predict(mod, newdata = new_data)$prob
    } else if (inherits(mod, "glm")) {
      prob_pos <- predict(mod, newdata = new_data, type = "response")
      probs <- matrix(c(1 - prob_pos, prob_pos), nrow = 1)
    } else if (inherits(mod, "multinom")) {
      probs <- predict(mod, newdata = new_data, type = "probs")
    } else if (inherits(mod, "kknn")) {
      if (is.null(mod$train_data)) {
        stop("Для kNN в стекинге не сохранены обучающие данные")
      }
      pred <- kknn::kknn(
        Class ~ .,
        train = mod$train_data,
        test = new_data,
        k = mod$k_value %||% 5,
        kernel = mod$kernel_value %||% "optimal"
      )
      probs <- pred$prob
    } else if (inherits(mod, "rpart")) {
      probs <- predict(mod, newdata = new_data, type = "prob")
    } else if (inherits(mod, "naiveBayes")) {
      probs <- predict(mod, newdata = new_data, type = "raw")
      if (!is.matrix(probs)) probs <- as.matrix(probs)
    } else if (inherits(mod, "lda")) {
      probs <- predict(mod, newdata = new_data)$posterior
    } else {
      stop(paste("Неизвестный тип базовой модели:", class(mod)[1]))
    }
    
    if (!is.matrix(probs)) {
      probs <- matrix(probs, nrow = 1)
    }
    colnames(probs) <- class_levels
    
    for (j in 1:ncol(probs)) {
      probs_list[[paste0(bm, "_", colnames(probs)[j])]] <- probs[, j]
    }
  }
  
  as.data.frame(probs_list)
}