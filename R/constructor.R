new_score_obj <- function(
  subclass = c("cat_num", "cat_cat", "num_num", "any"),
  outcome_type = c("numeric", "factor"),
  predictor_type = c("numeric", "factor"),
  case_weights = NULL,
  range = NULL,
  inclusive = NULL,
  fallback_value = NULL,
  score_type = NULL,
  trans = NULL,
  sorts = NULL,
  direction = NULL,
  deterministic = NULL,
  tuning = NULL,
  ties = NULL,
  calculating_fn = NULL,
  label = NULL,
  ...
) {
  # TODO Include validators here
  # TODO Add a validator to make sure subclass has to be in num_num, cat_num, cat_cat, any

  res <- list(
    outcome_type = outcome_type,
    predictor_type = predictor_type,
    case_weights = case_weights,
    range = range,
    inclusive = inclusive,
    fallback_value = fallback_value,
    score_type = score_type,
    trans = trans,
    sorts = sorts,
    direction = direction,
    deterministic = deterministic,
    tuning = tuning,
    ties = ties,
    calculating_fn = calculating_fn,
    label = label
  )
  class(res) <- c(subclass, "score_obj") # TODO Rename subclass

  res
}
