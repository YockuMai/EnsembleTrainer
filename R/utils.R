# R/utils.R
# Вспомогательные функции

# NULL-оператор: возвращает default, если x равен NULL
`%||%` <- function(x, default) {
  if (is.null(x)) default else x
}

# Безопасное приведение к числовому типу
safe_as_numeric <- function(x) {
  if (is.character(x) && x == "") return(NA_real_)
  as.numeric(x)
}

# Безопасное приведение к фактору с контролем уровней
safe_as_factor <- function(x, levels) {
  if (is.na(x) || !(x %in% levels)) {
    factor(NA, levels = levels)
  } else {
    factor(x, levels = levels)
  }
}

# Преобразование датафрейма в числовую матрицу (для XGBoost)
df_to_numeric_matrix <- function(df) {
  as.matrix(as.data.frame(lapply(df, function(col) {
    if (is.character(col)) as.numeric(as.factor(col)) else as.numeric(col)
  })))
}

# Проверка на бинарную классификацию
is_binary_classification <- function(y) {
  length(unique(y)) == 2
}