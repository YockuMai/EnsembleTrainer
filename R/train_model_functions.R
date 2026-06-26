# R/train_model_functions.R
# Чистые функции для обучения моделей

# Определяем оператор %||% для подстановки значений по умолчанию
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Разделение данных на обучающую и тестовую выборки
split_train_test <- function(data, target_col, train_ratio = 0.8, seed = 123) {
  set.seed(seed)
  train_indices <- sample(1:nrow(data), size = round(train_ratio * nrow(data)))
  list(
    train = data[train_indices, , drop = FALSE],
    test = data[-train_indices, , drop = FALSE],
    train_idx = train_indices,
    test_idx = setdiff(1:nrow(data), train_indices)
  )
}

#' Расчёт метрик для многоклассовой классификации
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
  
  # AUC (one-vs-rest)
  auc_vals <- numeric(length(classes))
  for (i in seq_along(classes)) {
    bin_true <- as.numeric(true == classes[i])
    if (length(unique(bin_true)) == 2) {
      auc_vals[i] <- pROC::auc(bin_true, probs[, i])
    } else auc_vals[i] <- NA
  }
  macro_auc <- mean(auc_vals, na.rm = TRUE)
  
  return(list(
    Accuracy = acc,
    Precision = macro_prec,
    Recall = macro_rec,
    F1 = macro_f1,
    AUC = macro_auc,
    ConfusionMatrix = table(pred, true)
  ))
}

#' Получить список доступных моделей с их функциями и параметрами
get_model_definitions <- function() {
  list(
    random_forest = list(
      name = "Случайный лес (randomForest)",
      method = "rf",
      train_func = function(formula, data, params) {
        ntree <- params$ntree %||% 500
        mtry <- params$mtry %||% floor(sqrt(ncol(data) - 1))
        nodesize <- params$nodesize %||% 1
        randomForest::randomForest(formula, data = data,
                                   ntree = ntree,
                                   mtry = mtry,
                                   nodesize = nodesize,
                                   importance = TRUE)
      },
      predict_func = function(model, newdata) {
        predict(model, newdata, type = "prob")
      },
      params = list(
        list(id = "ntree", label = "Количество деревьев", type = "numeric", default = 500, min = 10, max = 2000),
        list(id = "mtry", label = "Количество переменных для разделения", type = "numeric", default = NULL, min = 1, max = 50),
        list(id = "nodesize", label = "Минимальный размер листа", type = "numeric", default = 1, min = 1, max = 50)
      )
    ),
    extra_trees = list(
      name = "Extra Trees (ranger)",
      method = "ranger",
      train_func = function(formula, data, params) {
        num.trees <- params$num.trees %||% 500
        mtry <- params$mtry %||% floor(sqrt(ncol(data) - 1))
        min.node.size <- params$nodesize %||% 1
        ranger::ranger(formula, data = data,
                       num.trees = num.trees,
                       mtry = mtry,
                       min.node.size = min.node.size,
                       splitrule = "extratrees",
                       replace = FALSE,
                       sample.fraction = 1,
                       probability = TRUE)
      },
      predict_func = function(model, newdata) {
        predict(model, newdata)$predictions
      },
      params = list(
        list(id = "num.trees", label = "Количество деревьев", type = "numeric", default = 500, min = 10, max = 2000),
        list(id = "mtry", label = "Количество переменных для разделения", type = "numeric", default = NULL, min = 1, max = 50),
        list(id = "nodesize", label = "Минимальный размер листа", type = "numeric", default = 1, min = 1, max = 50)
      )
    ),
    gbm = list(
      name = "Gradient Boosting (gbm)",
      method = "gbm",
      train_func = function(formula, data, params) {
        n.trees <- params$n.trees %||% 500
        interaction.depth <- params$interaction.depth %||% 3
        shrinkage <- params$shrinkage %||% 0.01
        n.minobsinnode <- params$n.minobsinnode %||% 10
        gbm::gbm(formula, data = data,
                 n.trees = n.trees,
                 interaction.depth = interaction.depth,
                 shrinkage = shrinkage,
                 n.minobsinnode = n.minobsinnode,
                 bag.fraction = 0.5,
                 distribution = "multinomial",
                 verbose = FALSE)
      },
      predict_func = function(model, newdata) {
        pred_raw <- predict(model, newdata, n.trees = model$n.trees, type = "response")
        if (is.array(pred_raw) && length(dim(pred_raw)) == 3) {
          pred <- as.matrix(pred_raw[, , model$n.trees])
        } else {
          pred <- as.matrix(pred_raw)
        }
        colnames(pred) <- levels(model$data$y)
        pred
      },
      params = list(
        list(id = "n.trees", label = "Количество деревьев", type = "numeric", default = 500, min = 50, max = 2000),
        list(id = "interaction.depth", label = "Глубина взаимодействий", type = "numeric", default = 3, min = 1, max = 10),
        list(id = "shrinkage", label = "Скорость обучения", type = "numeric", default = 0.01, min = 0.001, max = 0.5)
      )
    ),
    xgboost = list(
      name = "XGBoost",
      method = "xgboost",
      train_func = function(formula, data, params) {
        target <- all.vars(formula)[1]
        y <- data[[target]]
        X <- data[, !names(data) %in% target]
        dtrain <- xgboost::xgb.DMatrix(data = as.matrix(X), label = as.numeric(y) - 1)
        nrounds <- params$nrounds %||% 500
        max_depth <- params$max_depth %||% 6
        eta <- params$eta %||% 0.1
        subsample <- params$subsample %||% 1
        colsample_bytree <- params$colsample_bytree %||% 1
        xgb_params <- list(
          objective = "multi:softprob",
          num_class = length(unique(y)),
          eval_metric = "mlogloss",
          max_depth = max_depth,
          eta = eta,
          subsample = subsample,
          colsample_bytree = colsample_bytree,
          min_child_weight = 1,
          gamma = 0,
          lambda = 1,
          alpha = 0
        )
        xgboost::xgb.train(params = xgb_params, data = dtrain, nrounds = nrounds, verbose = 0)
      },
      predict_func = function(model, newdata, target_levels) {
        dtest <- xgboost::xgb.DMatrix(data = as.matrix(newdata))
        probs <- predict(model, dtest, reshape = TRUE)
        colnames(probs) <- target_levels
        probs
      },
      params = list(
        list(id = "nrounds", label = "Количество итераций", type = "numeric", default = 500, min = 50, max = 2000),
        list(id = "max_depth", label = "Макс. глубина дерева", type = "numeric", default = 6, min = 2, max = 15),
        list(id = "eta", label = "Скорость обучения", type = "numeric", default = 0.1, min = 0.001, max = 0.5),
        list(id = "subsample", label = "Доля выборки", type = "numeric", default = 1, min = 0.5, max = 1, step = 0.1),
        list(id = "colsample_bytree", label = "Доля признаков", type = "numeric", default = 1, min = 0.5, max = 1, step = 0.1)
      )
    ),
    adaboost = list(
      name = "AdaBoost (adabag)",
      method = "adabag",
      train_func = function(formula, data, params) {
        mfinal <- params$mfinal %||% 200
        maxdepth <- params$maxdepth %||% 3
        coeflearn <- params$coeflearn %||% "Breiman"
        adabag::boosting(formula, data = data,
                         mfinal = mfinal,
                         control = rpart::rpart.control(maxdepth = maxdepth),
                         coeflearn = coeflearn)
      },
      predict_func = function(model, newdata) {
        pred <- predict(model, newdata)
        pred$prob
      },
      params = list(
        list(id = "mfinal", label = "Количество итераций", type = "numeric", default = 200, min = 20, max = 1000),
        list(id = "maxdepth", label = "Глубина деревьев", type = "numeric", default = 3, min = 1, max = 5),
        list(id = "coeflearn", label = "Коэффициент обучения", type = "select",
             choices = c("Breiman", "Freund", "Zhu"), default = "Breiman")
      )
    )
    # Стекинг пока опускаем для простоты
  )
}

#' Создать UI для параметров модели
create_param_ui <- function(model_def, ns_prefix) {
  params <- model_def$params
  if (is.null(params) || length(params) == 0) {
    return(p("Нет параметров для настройки"))
  }
  tagList(
    lapply(params, function(param) {
      param_id <- paste0(ns_prefix, "_", param$id)
      switch(param$type,
             numeric = {
               numericInput(param_id, param$label,
                            value = param$default,
                            min = param$min %||% NA,
                            max = param$max %||% NA,
                            step = param$step %||% 1)
             },
             logical = {
               checkboxInput(param_id, param$label, value = param$default %||% FALSE)
             },
             select = {
               selectInput(param_id, param$label,
                           choices = param$choices,
                           selected = param$default %||% param$choices[1])
             },
             {
               textInput(param_id, param$label, value = param$default)
             }
      )
    })
  )
}

#' Извлечь параметры из input
get_params_from_input <- function(input, model_def, ns_prefix) {
  params <- list()
  for (param in model_def$params) {
    param_id <- paste0(ns_prefix, "_", param$id)
    params[[param$id]] <- input[[param_id]]
  }
  params
}