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
  
  switch(model_id,
    "rf" = {
      mod <- randomForest::randomForest(x = X_train, y = y_train,
        ntree = params$ntree %||% 500, mtry = params$mtry %||% floor(sqrt(ncol(X_train))),
        nodesize = params$nodesize %||% 5, importance = FALSE)
      probs <- predict(mod, X_test, type = "prob")
      list(model = mod, metrics = calc_metrics(probs, y_test), probs = probs, class = "randomForest", label = "RandomForest")
    },
    "et" = {
      mod <- ranger::ranger(dependent.variable.name = target, data = train_data,
        num.trees = params$ntree %||% 500, mtry = params$mtry %||% floor(sqrt(ncol(train_data)-1)),
        min.node.size = params$nodesize %||% 5, splitrule = "extratrees",
        replace = FALSE, sample.fraction = 1, probability = TRUE, seed = 123)
      probs <- predict(mod, data = X_test)$predictions
      colnames(probs) <- levels(y_train)
      list(model = mod, metrics = calc_metrics(probs, y_test), probs = probs, class = "ranger", label = "ExtraTrees")
    },
    "gbm" = {
      mod <- gbm::gbm(stats::formula(paste(target, "~ .")), data = train_data,
        n.trees = params$n.trees %||% 500, interaction.depth = params$interaction.depth %||% 3,
        shrinkage = params$shrinkage %||% 0.1, bag.fraction = params$bag.fraction %||% 0.5,
        distribution = "multinomial", verbose = FALSE)
      p_raw <- predict(mod, X_test, n.trees = params$n.trees %||% 500, type = "response")
      if (is.array(p_raw) && length(dim(p_raw)) == 3) {
        n_trees_pred <- min(params$n.trees %||% 500, dim(p_raw)[3])
        slice <- p_raw[, , n_trees_pred]
        probs <- matrix(as.numeric(slice), nrow = dim(p_raw)[1], ncol = dim(p_raw)[2])
        dimnames(probs) <- NULL
      } else {
        probs <- as.matrix(p_raw)
        dimnames(probs) <- NULL
      }
      if (any(rowSums(probs, na.rm = TRUE) == 0)) {
        probs <- probs / rowSums(probs, na.rm = TRUE)
      }
      colnames(probs) <- levels(y_train)
      list(model = mod, metrics = calc_metrics(probs, y_test), probs = probs, class = "gbm", label = "GBM")
    },
    "xgb" = {
      X_train_num <- df_to_numeric_matrix(X_train); X_test_num <- df_to_numeric_matrix(X_test)
      if (any(is.na(X_train_num)) || any(is.na(X_test_num))) stop("NA в данных")
      dtrain <- xgboost::xgb.DMatrix(X_train_num, label = as.numeric(y_train)-1)
      dtest <- xgboost::xgb.DMatrix(X_test_num, label = as.numeric(y_test)-1)
      mod <- xgboost::xgb.train(list(objective = "multi:softprob", num_class = length(unique(y_train)),
        max_depth = params$max_depth %||% 6, eta = params$eta %||% 0.3, subsample = params$subsample %||% 1), dtrain,
        nrounds = params$nrounds %||% 100, verbose = 0)
      raw <- predict(mod, dtest)
      ncls <- length(levels(y_train))
      probs <- matrix(as.numeric(raw), nrow = nrow(X_test), ncol = ncls, byrow = TRUE)
      colnames(probs) <- levels(y_train)
      list(model = mod, metrics = calc_metrics(probs, y_test), probs = probs, class = "xgboost", label = "XGBoost")
    },
    "ada" = {
      if (is_binary_classification(y_train)) {
        mod <- ada::ada(stats::formula(paste(target, "~ .")), data = train_data,
          iter = params$mfinal %||% 100, loss = "exponential")
        probs <- predict(mod, newdata = test_data, type = "prob")
      } else {
        mod <- adabag::boosting(stats::formula(paste(target, "~ .")), data = train_data,
          mfinal = params$mfinal %||% 100, coeflearn = params$coeflearn %||% "Breiman",
          control = rpart::rpart.control(maxdepth = params$maxdepth %||% 2, minsplit = 10))
        probs <- predict(mod, newdata = test_data)$prob
      }
      colnames(probs) <- levels(y_train)
      list(model = mod, metrics = calc_metrics(probs, y_test), probs = probs, class = "ada", label = "AdaBoost")
    },
    "glm" = {
      if (is_binary_classification(y_train)) {
        mod <- glm(stats::formula(paste(target, "~ .")), data = train_data, family = binomial())
        probs <- cbind(1 - predict(mod, X_test, type = "response"), predict(mod, X_test, type = "response"))
      } else {
        mod <- nnet::multinom(stats::formula(paste(target, "~ .")), data = train_data, maxit = params$maxit %||% 100, trace = FALSE)
        probs <- predict(mod, X_test, type = "probs")
      }
      colnames(probs) <- levels(y_train)
      list(model = mod, metrics = calc_metrics(probs, y_test), probs = probs, class = "glm", label = "LogReg")
    },
    "knn" = {
      train_kknn <- cbind(Class = y_train, X_train)
      mod <- kknn::kknn(Class ~ ., train = train_kknn, test = cbind(Class = y_test, X_test), k = params$k %||% 5, kernel = params$kernel %||% "optimal")
      probs <- mod$prob; colnames(probs) <- levels(y_train)
      mod$train_data <- train_kknn; mod$k_value <- params$k %||% 5; mod$kernel_value <- params$kernel %||% "optimal"
      list(model = mod, metrics = calc_metrics(probs, y_test), probs = probs, class = "kknn", label = "kNN")
    },
    "rpart" = {
      mod <- rpart::rpart(stats::formula(paste(target, "~ .")), data = train_data,
        cp = params$cp %||% 0.01, minsplit = params$minsplit %||% 20, maxdepth = params$maxdepth %||% 30)
      probs <- predict(mod, X_test, type = "prob")
      list(model = mod, metrics = calc_metrics(probs, y_test), probs = probs, class = "rpart", label = "rpart")
    },
    "nb" = {
      mod <- e1071::naiveBayes(stats::formula(paste(target, "~ .")), data = train_data, laplace = params$fL %||% 0)
      probs <- predict(mod, X_test, type = "raw")
      if (!is.matrix(probs)) probs <- as.matrix(probs)
      colnames(probs) <- levels(y_train)
      list(model = mod, metrics = calc_metrics(probs, y_test), probs = probs, class = "naiveBayes", label = "NaiveBayes")
    },
    "lda" = {
      mod <- MASS::lda(stats::formula(paste(target, "~ .")), data = train_data)
      probs <- predict(mod, X_test)$posterior
      list(model = mod, metrics = calc_metrics(probs, y_test), probs = probs, class = "lda", label = "LDA")
    },
    stop(paste("Неизвестная модель:", model_id))
  )
}

# =============================================================================
# Предсказание одной обученной моделью
# =============================================================================
predict_with_model <- function(mod, new_data, model_id, model_params = list(), class_levels = NULL) {
  switch(model_id,
    "rf" = { pred <- predict(mod, newdata = new_data, type = "prob"); list(class = colnames(pred)[apply(pred,1,which.max)], prob = pred) },
    "et" = { pred <- predict(mod, data = new_data); p <- pred$predictions; list(class = colnames(p)[apply(p,1,which.max)], prob = p) },
    "gbm" = {
      n_trees <- model_params$n.trees %||% mod$n.trees %||% 100
      pred <- predict(mod, newdata = new_data, n.trees = n_trees, type = "response")
      if (is.array(pred) && length(dim(pred)) == 3) {
        n_trees <- model_params$n.trees %||% mod$n.trees %||% 100
        idx <- if (n_trees <= dim(pred)[3]) n_trees else 1
        slice_vec <- as.numeric(pred[, , idx])
        p <- matrix(slice_vec, nrow = dim(pred)[1], ncol = dim(pred)[2])
        dimnames(p) <- NULL
        cls <- class_levels %||% paste0("Class",1:ncol(p))
        colnames(p) <- cls
        list(class = cls[apply(p,1,which.max)], prob = p)
      } else if (is.matrix(pred)) {
        p <- as.matrix(pred); dimnames(p) <- NULL
        cls <- class_levels %||% colnames(p) %||% paste0("Class",1:ncol(p))
        colnames(p) <- cls
        list(class = cls[apply(p,1,which.max)], prob = p)
      } else {
        cls <- class_levels %||% c("0","1"); p <- matrix(pred, nrow=1)
        dimnames(p) <- NULL; colnames(p) <- cls
        list(class = cls[which.max(p[1,])], prob = p)
      }
    },
    "xgb" = {
      raw <- predict(mod, xgboost::xgb.DMatrix(df_to_numeric_matrix(new_data)))
      ncls <- length(class_levels %||% c("0","1"))
      p <- matrix(as.numeric(raw), ncol = ncls, byrow = TRUE)
      cls <- class_levels %||% paste0("Class", 1:ncls)
      colnames(p) <- cls
      list(class = cls[apply(p, 1, which.max)], prob = p)
    },
    "ada" = {
      if (inherits(mod, "ada")) {
        pred <- predict(mod, newdata = new_data, type = "prob")
        if (is.data.frame(pred)) pred <- as.matrix(pred)
        cls <- colnames(pred) %||% class_levels %||% paste0("Class", 1:ncol(pred))
        colnames(pred) <- cls
        list(class = cls[apply(pred, 1, which.max)], prob = pred)
      } else {
        pred <- predict(mod, newdata = new_data)
        if (is.data.frame(pred$prob)) pred$prob <- as.matrix(pred$prob)
        cls <- colnames(pred$prob) %||% class_levels %||% paste0("Class", 1:ncol(pred$prob))
        colnames(pred$prob) <- cls
        if (is.null(pred$class) || all(is.na(as.character(pred$class)))) {
          pred$class <- cls[apply(pred$prob, 1, which.max)]
        }
        list(class = as.character(pred$class), prob = pred$prob)
      }
    },
    "glm" = {
      if (inherits(mod, "glm")) {
        pp <- predict(mod, newdata = new_data, type = "response")
        cls <- class_levels %||% c("0","1"); p <- cbind(1-pp, pp); colnames(p) <- cls; list(class = cls[ifelse(pp>0.5,2,1)], prob = p)
      } else {
        pred <- predict(mod, newdata = new_data, type = "probs")
        if (!is.matrix(pred)) pred <- matrix(pred, nrow=1)
        cls <- colnames(pred) %||% class_levels %||% paste0("Class",1:ncol(pred))
        colnames(pred) <- cls; list(class = cls[apply(pred,1,which.max)], prob = pred)
      }
    },
    "knn" = {
      td <- model_params$train_data %||% mod$train_data
      kv <- model_params$k %||% mod$k_value %||% 5
      kr <- model_params$kernel %||% mod$kernel_value %||% "optimal"
      if (is.null(td)) stop("Для kNN не сохранены обучающие данные")
      pred <- kknn::kknn(Class ~ ., train = td, test = new_data, k = kv, kernel = kr)
      cls <- colnames(pred$prob) %||% class_levels %||% paste0("Class",1:ncol(pred$prob))
      colnames(pred$prob) <- cls; list(class = as.character(pred$fitted.values), prob = pred$prob)
    },
    "rpart" = { pred <- predict(mod, newdata = new_data, type = "prob"); list(class = colnames(pred)[apply(pred,1,which.max)], prob = pred) },
    "nb" = {
      pred <- predict(mod, newdata = new_data, type = "raw")
      if (!is.matrix(pred)) pred <- as.matrix(pred)
      cls <- colnames(pred) %||% class_levels %||% paste0("Class",1:ncol(pred))
      colnames(pred) <- cls; list(class = cls[apply(pred,1,which.max)], prob = pred)
    },
    "lda" = { pred <- predict(mod, newdata = new_data); list(class = as.character(pred$class), prob = pred$posterior) },
    "stack" = {
      # Поддерживаем оба формата: список-обёртка и атрибуты
      if (is.list(mod) && inherits(mod, "stacking_model")) {
        inner_model <- mod$meta_model
        stack_attr <- list(
          base_models_trained = mod$base_models_trained,
          meta_model = mod$meta_model_id,
          meta_params = mod$meta_params,
          target = mod$target,
          class_levels = mod$class_levels,
          meta_feature_cols = mod$meta_feature_cols
        )
      } else if (inherits(mod, "stacking_model")) {
        stack_attr <- attr(mod, "stack_metadata") %||% list(
          base_models_trained = mod$stack_base_models_trained,
          meta_model = mod$stack_meta_model,
          meta_params = mod$stack_meta_params,
          target = mod$stack_target,
          class_levels = mod$stack_class_levels
        )
        inner_model <- mod
      } else {
        stack_attr <- list(
          base_models_trained = mod$stack_base_models_trained,
          meta_model = mod$stack_meta_model,
          meta_params = mod$stack_meta_params,
          target = mod$stack_target,
          class_levels = mod$stack_class_levels
        )
        inner_model <- mod
      }
      if (!is.null(stack_attr$base_models_trained)) {
        # Генерируем мета-признаки через predict_stack_base_models
        stack_args <- list(
          base_models_trained = stack_attr$base_models_trained,
          new_data = new_data,
          class_levels = stack_attr$class_levels
        )
        if (!is.null(stack_attr$meta_feature_cols)) stack_args$meta_feature_cols <- stack_attr$meta_feature_cols
        meta_test <- do.call(predict_stack_base_models, stack_args)
        return(predict_with_model(inner_model, meta_test, stack_attr$meta_model %||% "glm", stack_attr$meta_params %||% list(), class_levels))
      }
      pred <- predict(inner_model, newdata = new_data)
      if (is.factor(pred)) { cls <- levels(pred); p <- matrix(0,1,length(cls), dimnames=list(NULL,cls)); p[1,as.character(pred)] <- 1; list(class=as.character(pred), prob=p) }
      else if (is.matrix(pred) || is.data.frame(pred)) { p <- as.matrix(pred); cls <- colnames(p) %||% class_levels %||% paste0("Class",1:ncol(p)); colnames(p) <- cls; list(class=cls[apply(p,1,which.max)], prob=p) }
      else { cls <- class_levels %||% c("0","1"); p <- cbind(1-as.numeric(pred), as.numeric(pred)); colnames(p) <- cls; list(class=cls[which.max(p[1,])], prob=p) }
    },
    stop(paste("Неизвестная модель для предсказания:", model_id))
  )
}

# =============================================================================
# Предсказание для стекинга: генерация мета-признаков из обученных базовых моделей
# =============================================================================
predict_stack_base_models <- function(base_models_trained, new_data, class_levels, meta_feature_cols = NULL) {
  probs_list <- list()
  for (bm in names(base_models_trained)) {
    mod <- base_models_trained[[bm]]
    probs <- NULL
    
    if (inherits(mod, "randomForest")) {
      probs <- predict(mod, newdata = new_data, type = "prob")
    } else if (inherits(mod, "ranger")) {
      probs <- predict(mod, data = new_data)$predictions
    } else if (inherits(mod, "gbm")) {
      nt <- if (!is.null(mod$n.trees)) mod$n.trees else 1000
      probs <- predict(mod, newdata = new_data, n.trees = nt, type = "response")
      if (is.array(probs) && length(dim(probs)) == 3) {
        idx <- if (nt <= dim(probs)[3]) nt else 1
        probs <- matrix(as.numeric(probs[, , idx]), nrow = dim(probs)[1], ncol = dim(probs)[2])
        dimnames(probs) <- NULL
      } else if (is.matrix(probs)) {
        probs <- as.matrix(probs)
        dimnames(probs) <- NULL
      } else if (is.numeric(probs)) {
        # fallback: вектор -> матрица по классам
        ncls <- length(class_levels)
        probs <- matrix(probs, ncol = ncls, byrow = TRUE)
      }
    } else if (inherits(mod, "ada")) {
      probs <- predict(mod, newdata = new_data, type = "prob")
    } else if (inherits(mod, "boosting")) {
      probs <- predict(mod, newdata = new_data)$prob
    } else if (inherits(mod, "glm")) {
      pp <- predict(mod, newdata = new_data, type = "response")
      probs <- cbind(1-pp, pp)
    } else if (inherits(mod, "multinom")) {
      probs <- predict(mod, newdata = new_data, type = "probs")
    } else if (inherits(mod, "kknn")) {
      if (is.null(mod$train_data)) stop("Для kNN в стекинге не сохранены обучающие данные")
      pred <- kknn::kknn(Class ~ ., train = mod$train_data, test = new_data,
        k = mod$k_value %||% 5, kernel = mod$kernel_value %||% "optimal")
      probs <- pred$prob
    } else if (inherits(mod, "rpart")) {
      probs <- predict(mod, newdata = new_data, type = "prob")
    } else if (inherits(mod, "naiveBayes")) {
      probs <- predict(mod, newdata = new_data, type = "raw")
      if (!is.matrix(probs)) probs <- as.matrix(probs)
    } else if (inherits(mod, "lda")) {
      probs <- predict(mod, newdata = new_data)$posterior
    } else if (inherits(mod, "xgb.Booster")) {
      raw <- predict(mod, xgboost::xgb.DMatrix(df_to_numeric_matrix(new_data)))
      class_levels <- class_levels %||% paste0("Class", 1:length(unique(raw)))
      ncls <- length(class_levels)
      probs <- matrix(as.numeric(raw), ncol = ncls, byrow = TRUE)
    } else {
      stop(paste("Неизвестный тип базовой модели:", class(mod)[1]))
    }
    
    if (is.null(probs)) next
    # Safe local normalization fallback
    canon <- tryCatch(normalize_prob_matrix(probs, class_levels, bm), error = function(e) NULL)
    if (is.null(canon)) {
      # Manual fallback: coerce to matrix and take first n_classes columns
      mat <- as.matrix(probs)
      ncls <- length(class_levels)
      canon <- matrix(0, nrow = NROW(mat), ncol = ncls)
      n_copy <- min(ncol(mat), ncls)
      canon[, 1:n_copy] <- mat[, 1:n_copy, drop = FALSE]
    }
    
    for (j in seq_along(class_levels)) {
      probs_list[[paste0(bm, "_", class_levels[j])]] <- canon[, j]
    }
  }
  # Ensure each column vector matches new_data row count exactly
  n_target <- NROW(new_data)
  for (nm in names(probs_list)) {
    v <- probs_list[[nm]]
    if (length(v) != n_target) {
      if (length(v) > n_target) {
        probs_list[[nm]] <- v[1:n_target]
      } else {
        probs_list[[nm]] <- c(v, rep(0, n_target - length(v)))[1:n_target]
      }
    }
  }
  # Строгое соответствие колонок: создаём из списка значений по каноническому списку имён
  out <- as.data.frame(matrix(0, nrow = n_target, ncol = length(meta_feature_cols), dimnames = list(NULL, meta_feature_cols)))
  for (j in seq_along(probs_list)) {
    nm <- names(probs_list)[j]
    if (nm %in% meta_feature_cols) out[[nm]] <- probs_list[[j]]
  }
  
  out
}
