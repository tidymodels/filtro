filter_aov <- function(
  range = c(0, Inf),
  trans = NULL,
  direction = "maximize"
) {
  new_num_cat_score(
    outcome_type = c("double", "integer", "character", "logical"),
    predictor_type = c("double", "integer", "character", "logical"),
    case_weights = case_weights,
    range = range,
    inclusive = c(TRUE, TRUE),
    fallback_values = Inf,
    score_type = score_type,
    trans = trans,
    sorts = sorts,
    direction = direction,
    deterministic = TRUE,
    tuning = FALSE,
    ties = ties,
    label = c(filter_aov = "Filter method based on ANOVA F-statistics"),
  )
}
