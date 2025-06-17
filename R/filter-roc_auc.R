filter_roc_auc <- function(range = c(0, 1), trans = NULL) {
  new_cat_num_score(
    outcome_type = c("numeric", "factor"),
    predictor_type = c("numeric", "factor"),
    case_weights = case_weights,
    range = range,
    inclusive = c(TRUE, TRUE),
    fallback_value = 1,
    score_type = score_type,
    trans = trans,
    sorts = sorts,
    direction = "maximize",
    deterministic = TRUE,
    tuning = FALSE,
    ties = ties,
    label = c(filter_aov = "Filter method based on ROC AUC scores")
  )
}

get_roc_auc <- function(x, y) {
  # Helper function to check if a vector is a factor with 2+ levels
  is_factor <- function(v) is.factor(v) && length(levels(v)) >= 2

  if (is_factor(y) && is.numeric(x)) {
    outcome <- y
    predictor <- x
  } else if (is.numeric(y) && is_factor(x)) {
    outcome <- x
    predictor <- y
  } else {
    stop(
      "One argument must be a factor with 2+ levels and the other numeric."
    )
  }

  if (length(levels(outcome)) == 2) {
    roc <- pROC::roc(outcome, predictor, direction = "auto")
    res <- pROC::auc(roc)
  } else {
    roc <- pROC::multiclass.roc(outcome, predictor, direction = "auto")
    res <- pROC::auc(roc)
  }
  res
}
