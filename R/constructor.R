new_num_cat_score <- function(
  outcome_type = c("double", "integer", "character", "logical"),
  predictor_type = c("double", "integer", "character", "logical"),
  case_weights = NULL,
  range = NULL,
  inclusive = NULL,
  fallback_values = NULL,
  score_type = NULL,
  trans = NULL,
  sorts = NULL,
  direction = NULL,
  deterministic = NULL,
  tuning = NULL,
  ties = NULL,
  label = NULL,
  ...
) {
  # To do: Include validators here

  res <- list(
    outcome_type = outcome_type,
    predictor_type = predictor_type,
    case_weights = case_weights,
    range = range,
    inclusive = inclusive,
    fallback_values = fallback_values,
    score_type = score_type,
    trans = trans,
    sorts = sorts,
    direction = direction,
    deterministic = deterministic,
    tuning = tuning,
    ties = ties,
    label = label
  )
  class(res) <- c("num_cat_score", "score")

  res
}
