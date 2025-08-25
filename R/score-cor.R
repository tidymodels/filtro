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
#' These two objects can be used to compute importance scores based on
#' correlation coefficient.
#'
#' @name score_cor_pearson
#' @include utilities.R
#'
#' @family class score metrics
#' @details
#' These objects are used when:
#'
#' - The predictors are numeric and the outcome is numeric.
#'
#' In this case, a correlation coefficient (via [stats::cov.wt()]) is computed with
#' the proper variable roles. Values closer to 1 or -1 (i.e., `abs(cor_pearson)`
#' closer to 1) are associated with more important predictors.
#'
#' ## Estimating the scores
#'
#' In \pkg{filtro}, the `score_*` objects define a scoring method (e.g., data
#' input requirements, package dependencies, etc). To compute the scores for
#' a specific data set, the `fit()` method is used. The main arguments for
#' these functions are:
#'
#'   \describe{
#'     \item{`object`}{A score class object (e.g., `score_cor_pearson`).}
#'     \item{`formula`}{A standard R formula with a single outcome on the right-hand side and one or more predictors (or `.`) on the left-hand side. The data are processed via [stats::model.frame()]}
#'     \item{`data`}{A data frame containing the relevant columns defined by the formula.}
#'     \item{`...`}{Further arguments passed to or from other methods.}
#'     \item{`case_weights`}{A quantitative vector of case weights that is the same length as the number of rows in `data`. The default of `NULL` indicates that there are no case weights.}
#'   }
#'
#' @includeRmd man/rmd/missing_delete.Rmd details
#'
#' @includeRmd man/rmd/fault_tolerant.Rmd details
#'
#' @return An S7 object. The primary property of interest is in `results`. This
#' is a data frame of results that is populated by the `fit()` method and has
#' columns:
#'
#' - `name`: The name of the score (e.g., `score_cor_pearson` or `score_cor_spearman`).
#' - `score`: The estimates for each predictor.
#' - `outcome`: The name of the outcome column.
#' - `predictor`: The names of the predictor inputs.
#'
#' These data are accessed using `object@results` (see examples below).
#'
#' @examplesIf rlang::is_installed("modeldata")
#' library(dplyr)
#'
#' ames <- modeldata::ames
#'
#' # Pearson correlation
#' ames_cor_pearson_res <-
#'   score_cor_pearson |>
#'   fit(Sale_Price ~ ., data = ames)
#' ames_cor_pearson_res@results
#'
#' # Spearman correlation
#' ames_cor_spearman_res <-
#'   score_cor_spearman |>
#'   fit(Sale_Price ~ ., data = ames)
#' ames_cor_spearman_res@results
#' @export
score_cor_pearson <-
  class_score_cor(
    outcome_type = "numeric",
    predictor_type = "numeric",
    case_weights = TRUE,
    range = c(-1, 1),
    inclusive = c(TRUE, TRUE),
    fallback_value = 1,
    score_type = "cor_pearson",
    transform_fn = filtro_abs_trans,
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
    case_weights = TRUE,
    range = c(-1, 1),
    inclusive = c(TRUE, TRUE),
    fallback_value = 1,
    score_type = "cor_spearman",
    transform_fn = filtro_abs_trans,
    direction = "maximize",
    deterministic = TRUE,
    tuning = FALSE,
    label = "Spearman's rank correlation coefficient"
  )

# ------------------------------------------------------------------------------

#' @export
S7::method(fit, class_score_cor) <- function(
  object,
  formula,
  data,
  case_weights = NULL,
  ...
) {
  analysis_data <- process_all_data(formula, data = data)
  predictors <- names(analysis_data)[-1]
  outcome <- names(analysis_data)[1]
  case_weights <- convert_weights(case_weights, nrow(analysis_data))

  compete_obs <- !is.na(analysis_data[outcome])
  if (!is.null(case_weights)) {
    compete_obs <- compete_obs & !is.na(case_weights)
  }
  analysis_data <- analysis_data[compete_obs, ]

  if (object@score_type == "cor_spearman") {
    analysis_data <- dplyr::mutate(
      analysis_data,
      dplyr::across(dplyr::where(is.numeric), ~ rank(.x, na.last = "keep"))
    )
  }

  score <- purrr::map_dbl(
    purrr::set_names(predictors),
    \(x) {
      map_score_cor(
        analysis_data,
        predictor = x,
        outcome = outcome,
        weights = case_weights
      )
    }
  )
  res <- named_vec_to_tibble(score, object@score_type, outcome)

  object@results <- res
  object
}

map_score_cor <- function(data, predictor, outcome, weights) {
  predictor_col <- data[[predictor]]
  outcome_col <- data[[outcome]]

  if (is.factor(outcome_col) || is.factor(predictor_col)) {
    return(NA_real_)
  }

  compete_obs <- !is.na(predictor_col)
  outcome_col <- outcome_col[compete_obs]
  predictor_col <- predictor_col[compete_obs]

  if (!is.null(weights)) {
    weights <- weights[compete_obs]
  } else {
    weights <- rep(1.0, length(outcome_col))
  }

  res <- try(
    stats::cov.wt(
      cbind(outcome_col, predictor_col),
      wt = weights,
      method = "ML",
      cor = TRUE
    )$cor,
    silent = TRUE
  )

  if (inherits(res, "try-error")) {
    res <- NA_real_
  } else {
    res <- res[1, 2]
  }
  res
}
