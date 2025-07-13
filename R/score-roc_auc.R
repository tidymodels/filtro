#' Create a score object for area under the Receiver Operating Characteristic curve (ROC AUC)
#'
#' Construct a score object containing metadata for univariate feature scoring using the
#' the Receiver Operating Characteristic curve (ROC AUC).
#' Output a score object containing associated metadata such as `range`, `fallback_value`,
#' `score_type` (`"roc_auc"`), `direction`, and other relevant attributes.
#'
#' @inheritParams new_score_obj
#' @param fallback_value A numeric scalar used as a fallback value. One of:
#'    - `1` (default)
#' @param score_type A character string indicating the type of scoring metric to compute.
#' One of:
#'    - `"roc_auc"` (default)
#' @param direction A character string indicating the optimization direction. One of:
#'  - `"maximize"` (default)
#'  - `"minimize"`
#'  - `"target"`
#'
#' @returns A score object containing associated metadata such as `range`, `fallback_value`,
#' `score_type` (`"roc_auc"`), `direction`, and other relevant attributes.
#'
#' @export
#'
#' @examples
#' # Create a score object
#' score_roc_auc()
score_roc_auc <- function(
  range = c(0, 1),
  fallback_value = 1,
  score_type = "roc_auc",
  direction = "maximize"
) {
  new_score_obj(
    outcome_type = c("numeric", "factor"),
    predictor_type = c("numeric", "factor"),
    case_weights = FALSE,
    range = range,
    inclusive = c(TRUE, TRUE),
    fallback_value = fallback_value,
    score_type = score_type,
    #trans = # Cannot set NULL. Otherwise S7 complains
    #sorts =
    direction = direction,
    deterministic = TRUE,
    tuning = FALSE,
    #ties =
    calculating_fn = function(x) {}, # Otherwise S7 complains
    label = c(score_rocauc = "ROC AUC scores")
  )
}

flip_if_needed_roc_auc <- function(predictor, outcome) {
  # TODO Move to utilities.R
  if (is.factor(outcome) && is.numeric(predictor)) {
    list(predictor = predictor, outcome = outcome)
  } else {
    list(predictor = outcome, outcome = predictor)
  }
}

get_single_roc_auc <- function(predictor, outcome, ...) {
  flipped <- flip_if_needed_roc_auc(predictor, outcome)
  outcome <- flipped$outcome
  predictor <- flipped$predictor

  if (length(levels(outcome)) == 2) {
    # TODO if else will change once we pass case_weights = in later
    roc <- pROC::roc(outcome, predictor, direction = "auto", quiet = TRUE, ...)
  } else {
    roc <- pROC::multiclass.roc(
      outcome,
      predictor,
      direction = "auto",
      quiet = TRUE,
      ...
    )
  }
  res <- pROC::auc(roc) |> as.numeric()
  res
}

map_score_roc_auc <- function(data, predictor, outcome) {
  predictor_col <- data[[predictor]]
  outcome_col <- data[[outcome]]

  if (is.factor(outcome_col) && !is.numeric(predictor_col)) {
    return(NA_real_)
  }

  if (is.numeric(outcome_col) && !is.factor(predictor_col)) {
    return(NA_real_)
  }

  res <- get_single_roc_auc(predictor_col, outcome_col)
  res
}

make_scores_roc_auc <- function(score_type, score, outcome, predictors) {
  res <- tibble::tibble(
    name = score_type,
    score = unname(score),
    outcome = outcome,
    predictor = predictors
  )
  res
}

#' Compute area under the Receiver Operating Characteristic curve (ROC AUC)
#'
#' Evaluate the relationship between a numeric outcome and a categorical predictor,
#' or vice versa, by computing the area under the Receiver Operating Characteristic curve (ROC AUC).
#' Output a tibble result with with one row per predictor, and four columns:
#' `name`, `score`, `predictor`, and `outcome`.
#'
#' @param score_obj A score object. See [score_roc_auc()] for details.
#'
#' @param data A data frame or tibble containing the outcome and predictor variables.
#' @param outcome A character string specifying the name of the outcome variable.
#' @param ... NULL
#'
#' @return A tibble of result with one row per predictor, and four columns:
#' - `name`: the name of scoring metric.
#' - `score`: the score for the predictor-outcome pair.
#' - `predictor`: the name of the predictor.
#' - `outcome`: the name of the outcome.
#'
#' @export
#'
get_scores_roc_auc <- function(score_obj, data, outcome) {
  predictors <- setdiff(names(data), outcome)

  score <- purrr::map_dbl(
    purrr::set_names(predictors),
    \(x) map_score_roc_auc(data, x, outcome)
  )

  res <- make_scores_roc_auc(score_obj@score_type, score, outcome, predictors)
  res
}
