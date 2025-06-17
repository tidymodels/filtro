filter_roc_auc <- function(range = c(0, 1), trans = NULL) {
  new_num_cat_score(
    outcome_type = c("double", "integer", "character", "logical"),
    predictor_type = c("double", "integer", "character", "logical"),
    case_weights = case_weights,
    range = range,
    inclusive = c(TRUE, TRUE),
    fallback_values = 1,
    score_type = score_type,
    trans = trans,
    sorts = sorts,
    direction = "maximize",
    deterministic = TRUE,
    tuning = FALSE,
    ties = ties,
    label = c(filter_aov = "Filter method based on ROC AUC scores"),
  )
}
