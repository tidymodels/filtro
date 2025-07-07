#' Compute information gain
#'
#' @param range NULL
#' @param trans NULL
#' @param score_type NULL
#' @param direction NULL
#'
#' @returns NULL
#' @export
#'
#' @examples NULL
score_info_gain <- function(
  range = c(0, Inf),
  trans = NULL,
  score_type = "infogain", # Move c() here later. Add validator. Document it.
  direction = "maximize"
) {
  new_score_obj(
    subclass = c("any"),
    outcome_type = c("numeric", "factor"),
    predictor_type = c("numeric", "factor"),
    case_weights = FALSE, # TODO
    range = range,
    inclusive = c(TRUE, TRUE),
    fallback_value = Inf,
    score_type = score_type, # c("infogain", "gainratio", "symuncert"),
    trans = NULL, # TODO
    sorts = NULL, # TODO
    direction = c("maximize", "minimize", "target"),
    deterministic = TRUE,
    tuning = FALSE,
    ties = NULL,
    calculating_fn = NULL,
    label = c(score_infogain = "Information Gain scores")
  )
}

make_scores_info_gain <- function(
  score_type,
  fit,
  outcome
) {
  res <- dplyr::tibble(
    name = score_type,
    score = fit$importance,
    outcome = outcome,
    predictor = fit$attributes
  )
  res
}

#' @export
get_scores_info_gain <- function(
  score_obj,
  data,
  outcome,
  ...
) {
  y <- data[[outcome]]
  X <- data[setdiff(names(data), outcome)]

  fit <- FSelectorRcpp::information_gain(
    x = X,
    y = y,
    type = "infogain", # TODO
    equal = score_obj$equal # Set = TRUE for numeric outcome
  )
  res <- make_scores_info_gain(score_obj$score_type, fit, outcome)
  res
}
