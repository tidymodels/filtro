#' @rdname class_score
#' @include class_score.R
#' @keywords internal
#' @export
class_score_cor <- S7::new_class(
  "class_score_cor",
  parent = class_score
)

#' Scoring via correlation coefficient
#'
#' @description
#'
#' These objects are used when:
#'
#' - The predictors are numeric and the outcome is numeric.
#'
#' In this case, a correlation coefficient (via [stats::cor()]) is computed with the proper
#' variable roles.
#'
#' `score_cor_pearson` and `score_cor_spearman` are objects that define the technique.
#' To apply the filter on data, you would use the [fit()] method:
#'
#' \preformatted{
#'   fit(score_cor_pearson, formula, data)
#' }
#'
#' See the Examples section below.
#' @name score_cor_pearson
#' @export
score_cor_pearson <-
  class_score_cor(
    outcome_type = "numeric",
    predictor_type = "numeric",
    case_weights = FALSE,
    range = c(-1, 1),
    inclusive = c(TRUE, TRUE),
    fallback_value = 1,
    score_type = "cor_pearson",
    direction = "maximize",
    deterministic = TRUE,
    tuning = FALSE,
    label = "Pearson correlation coefficient"
  )

#' @name score_cor_pearson
#' @export
score_cor_spearman <-
  class_score_cor(
    outcome_type = "numeric",
    predictor_type = "numeric",
    case_weights = FALSE,
    range = c(-1, 1),
    inclusive = c(TRUE, TRUE),
    fallback_value = 1,
    score_type = "cor_spearman",
    direction = "maximize",
    deterministic = TRUE,
    tuning = FALSE,
    label = "Spearman's rank correlation coefficient"
  )

# ------------------------------------------------------------------------------

#' Compute correlation coefficients
#' @name score_cor_pearson
#' @include class_score.R
#' @param object A score class object based on `class_score_cor`.
#' @param formula A standard R formula with a single outcome on the right-hand
#' side and one or more predictors (or `.`) on the left-hand side. The data are
#' processed via [stats::model.frame()].
#' @param data A data frame containing the relevant columns defined by the
#' formula.
#' @param ... Further arguments passed to or from other methods.
#' @details
#' The function will determine which columns are predictors and outcomes and
#' compute correlations; no user intervention is required.
#'
#' Missing values are removed for each predictor/outcome combination being
#' scored.
#' In cases where [cor()] fail, the scoring proceeds silently, and
#' a missing value is given for the score.
#'
#' @examples
#' if (rlang::is_installed("modeldata")) {
#'
#'   library(dplyr)
#'
#'   ames_subset <- modeldata::ames |>
#'     select(
#'       Sale_Price,
#'       MS_SubClass,
#'       MS_Zoning,
#'       Lot_Frontage,
#'       Lot_Area,
#'       Street
#'     )
#'
#'   ames_cor_pearson_res <-
#'     score_cor_pearson |>
#'     fit(Sale_Price ~ ., data = ames_subset)
#'   ames_cor_pearson_res@results
#'
#'   ames_cor_spearman_res <-
#'     score_cor_spearman |>
#'     fit(Sale_Price ~ ., data = ames_subset)
#'   ames_cor_spearman_res@results
#' }
#' @export
S7::method(fit, class_score_cor) <- function(object, formula, data, ...) {
  analysis_data <- process_all_data(formula, data = data)
  predictors <- names(analysis_data)[-1]
  outcome <- names(analysis_data)[1]

  if (object@score_type == "cor_pearson") {
    object@calculating_fn <- get_single_pearson
  } else if (object@score_type == "cor_spearman") {
    object@calculating_fn <- get_single_spearman
  }
  score <- purrr::map_dbl(
    purrr::set_names(predictors),
    \(x) {
      map_score_cor(
        data,
        predictor = x,
        outcome = outcome,
        calculating_fn = object@calculating_fn
      )
    }
  )
  res <- named_vec_to_tibble(score, object@score_type, outcome)

  object@results <- res
  object
}

map_score_cor <- function(data, predictor, outcome, calculating_fn) {
  predictor_col <- data[[predictor]]
  outcome_col <- data[[outcome]]

  if (is.factor(outcome_col) || is.factor(predictor_col)) {
    return(NA_real_)
  }

  res <- calculating_fn(predictor_col, outcome_col)
  res
}

get_single_pearson <- function(predictor, outcome) {
  res <- stats::cor(predictor, outcome, method = "pearson")
  return(res)
}

get_single_spearman <- function(predictor, outcome) {
  res <- stats::cor(predictor, outcome, method = "spearman")
  return(res)
}
