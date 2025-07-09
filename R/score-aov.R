#' Create a score object for ANOVA F-test F-statistics and p-values
#'
#' Construct a score object containing metadata for univariate feature scoring using the
#' ANOVA F-test.
#' Output a score object containing associated metadata such as `range`, `fallback_value`,
#' `score_type` (`"fstat"` or `"pval"`), `direction`, and other relevant attributes.
#'
#' @inheritParams new_score_obj
#' @param fallback_value A numeric scalar used as a fallback value. Typical values
#' include:
#'    - `0`
#'   - `Inf` (default)
#'
#' For F-statistics, the `fallback_value` should be `"Inf"`. For p-values,
#' since the default applies a negative log10 transformation to p-values,
#' the `fallback_value` should be `"Inf"`.
#'
#' @param score_type A character string indicating the type of scoring metric to compute.
#' Available options include:
#'    - `"fstat"`
#'    - `"pval"`
#' @param direction A character string indicating the optimization direction. One of:
#'  - `"maximize"` (default)
#'  - `"minimize"`
#'  - `"target"`
#'
#' For F-statistics, the `direction` should be `"maximize"`. For p-values,
#' since the default applies a negative log10 transformation to p-values,
#' the `direction` should be `"maximize"`.
#'
#' @returns A score object containing associated metadata such as `range`, `fallback_value`,
#' `score_type` (`"fstat"` or `"pval"`), `direction`, and other relevant attributes.
#'
#' @export
#'
#' @examples
#' # Create a score object
#' score_aov()
#' # Change score type
#' score_obj <- score_aov()
#' score_obj$score_type <- "pval"
score_aov <- function(
  range = c(0, Inf),
  fallback_value = Inf,
  score_type = "fstat",
  direction = "maximize"
) {
  #fallback_value <- rlang::arg_match0(fallback_value, c(0, Inf))
  score_type <- rlang::arg_match0(score_type, c("fstat", "pval"))
  direction <- rlang::arg_match0(direction, c("maximize", "minimize", "target"))

  new_score_obj(
    subclass = c("any"),
    outcome_type = c("numeric", "factor"),
    predictor_type = c("numeric", "factor"),
    case_weights = FALSE, # TODO
    range = range,
    inclusive = c(TRUE, TRUE),
    fallback_value = fallback_value,
    score_type = score_type,
    trans = NULL, # TODO
    sorts = NULL, # TODO
    direction = direction,
    deterministic = TRUE,
    tuning = FALSE,
    ties = NULL,
    calculating_fn = NULL,
    label = c(score_aov = "ANOVA F-test F-statistics and p-values")
  )
}

flip_if_needed_aov <- function(predictor, outcome) {
  # TODO Move to utilities.R
  if (is.factor(outcome) && is.numeric(predictor)) {
    list(predictor = outcome, outcome = predictor)
  } else {
    list(predictor = predictor, outcome = outcome)
  }
}

get_single_f_stat <- function(predictor, outcome) {
  flipped <- flip_if_needed_aov(predictor, outcome)
  outcome <- flipped$outcome
  predictor <- flipped$predictor

  fit <- stats::lm(outcome ~ predictor)
  res <- stats::anova(fit)$`F value`[1] # summary(fit)$fstatistic[1] |> as.numeric()
  return(res)
}

get_single_p_val <- function(predictor, outcome) {
  flipped <- flip_if_needed_aov(predictor, outcome)
  outcome <- flipped$outcome
  predictor <- flipped$predictor

  fit <- stats::lm(outcome ~ predictor)
  res <- stats::anova(fit)$`Pr(>F)`[1]
  return(res)
}

map_score_aov <- function(data, predictor, outcome, calculating_fn) {
  predictor_col <- data[[predictor]]
  outcome_col <- data[[outcome]]

  if (is.factor(outcome_col) && !is.numeric(predictor_col)) {
    return(NA_real_)
  }

  if (is.numeric(outcome_col) && !is.factor(predictor_col)) {
    return(NA_real_)
  }

  res <- calculating_fn(predictor_col, outcome_col)
  res
}

make_scores_aov <- function(score_type, score, outcome, predictors) {
  res <- tibble::tibble(
    name = score_type,
    score = unname(score),
    outcome = outcome,
    predictor = predictors
  )
  res
}

#' Compute F-statistic and p-value scores using ANOVA F-test
#'
#' Evaluate the relationship between a numeric outcome and a categorical predictor,
#' or vice versa, by computing the ANOVA F-statistic or p-value.
#' Output a tibble result with with one row per predictor, and four columns:
#' `name`, `score`, `predictor`, and `outcome`.
#'
#' @param score_obj A score object. See [score_aov()] for details.
#' @details
#' The `score_obj` object may include the following components:
#' \describe{
#'   \item{`neg_log10`}{A logical value indicating whether to apply a negative log10
#'   transformation to p-values (default is `TRUE`).
#'   - If `TRUE`, p-values are transformed as `-log10(pval)`. In this case:
#'     - The default `fallback_value` is `Inf`
#'     - The default `direction` is `"maximize"`
#'   - If `FALSE`, raw p-values are used. In this case:
#'     - The `fallback_value` should be set to `0`
#'     - The `direction` should be set to `"minimize"`
#'   }
#' }
#' @param data A data frame or tibble containing the outcome and predictor variables.
#' @param outcome A character string specifying the name of the outcome variable.
#'
#' @return A tibble of result with one row per predictor, and four columns:
#' - `name`: the name of scoring metric.
#' - `score`: the score for the predictor-outcome pair.
#' - `predictor`: the name of the predictor.
#' - `outcome`: the name of the outcome.
#'
#' @export
#'
#' @examples
#' data(ames, package = "modeldata")
#' data <- modeldata::ames |>
#'   dplyr::select(
#'     Sale_Price,
#'     MS_SubClass,
#'     MS_Zoning,
#'     Lot_Frontage,
#'     Lot_Area,
#'     Street
#'   )
#' # Define outcome
#' outcome <- "Sale_Price"
#' # Create a score object
#' score_obj <- score_aov()
#' score_res <- get_scores_aov(score_obj, data, outcome)
#' score_res
#' # Change score type
#' score_obj$score_type <- "pval"
#' score_res <- get_scores_aov(score_obj, data, outcome)
#' score_res
#' # Use raw p-values instead of -log10(p-values)
#' score_obj$score_type <- "pval"
#' score_obj$neg_log10 <- FALSE
#' score_obj$direction <- "minimize"
#' score_obj$fallback_value <- 0
#' score_res <- get_scores_aov(score_obj, data, outcome)
#' score_res
get_scores_aov <- function(score_obj, data, outcome) {
  if (score_obj$score_type == "fstat") {
    score_obj$calculating_fn <- get_single_f_stat
  } else if (score_obj$score_type == "pval") {
    score_obj$calculating_fn <- get_single_p_val
  }

  predictors <- setdiff(names(data), outcome)

  score <- purrr::map_dbl(
    purrr::set_names(predictors),
    \(x) map_score_aov(data, x, outcome, score_obj$calculating_fn)
  )

  # Do we need score <- stats::p.adjust(score) here too?

  if (
    score_obj$score_type == "pval" &&
      (is.null(score_obj$neg_log10) || isTRUE(score_obj$neg_log10))
  ) {
    score <- -log10(score)
  }

  res <- make_scores_aov(score_obj$score_type, score, outcome, predictors)
  res
}
