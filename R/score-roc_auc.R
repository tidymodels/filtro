#' @rdname class_score
#' @include class_score.R
#' @keywords internal
#' @export
class_score_roc_auc <- S7::new_class(
  "class_score_roc_auc",
  parent = class_score
)

#' Scoring via area under the Receiver Operating Characteristic curve (ROC AUC)
#'
#' @description
#'
#' These objects are used when either:
#'
#' - The predictors are numeric and the outcome is a factor/category, or
#' - The predictors are factors and the outcome is numeric.
#'
#' In either case, a ROC curve (via [pROC::roc()]) is created with the proper
#' variable roles, and the area under the ROC curve is computed (via [pROC::auc()]).
#' Values higher than 0.5 (i.e., `max(roc_auc, 1 - roc_auc)` > 0.5) are associated with
#' more important predictors.
#'
#' `score_roc_auc` is object that define the technique.
#' To apply the filter on data, you would use the [fit()] method:
#'
#' \preformatted{
#'   fit(score_roc_auc, formula, data)
#' }
#'
#' See the Examples section below.
#' @name score_roc_auc
#' @export
score_roc_auc <-
  class_score_roc_auc(
    outcome_type = c("numeric", "factor"),
    predictor_type = c("numeric", "factor"),
    case_weights = TRUE, # TODO
    range = c(0, 1),
    inclusive = c(TRUE, TRUE),
    fallback_value = 1,
    score_type = "roc_auc",
    direction = "maximize",
    deterministic = TRUE,
    tuning = FALSE,
    label = "Area under the Receiver Operating Characteristic curve (ROC AUC)"
  )

# ------------------------------------------------------------------------------

#' Compute area under the Receiver Operating Characteristic curve (ROC AUC)
#' @name score_roc_auc
#' @include class_score.R
#' @param object A score class object based on `class_score_roc_auc`.
#' @param formula A standard R formula with a single outcome on the right-hand
#' side and one or more predictors (or `.`) on the left-hand side. The data are
#' processed via [stats::model.frame()].
#' @param data A data frame containing the relevant columns defined by the
#' formula.
#' @param ... Further arguments passed to or from other methods.
#' @details
#' The function will determine which columns are predictors and outcomes for the
#' ROC analysis; no user intervention is required.
#'
#' Missing values are removed for each predictor/outcome combination being
#' scored.
#' In cases where [pROC::roc()] fail, the scoring proceeds silently, and
#' a missing value is given for the score.
#'
#' @examplesIf rlang::is_installed("modeldata")
#'
#' library(dplyr)
#'
#' # ROC AUC where the numeric predictors are the predictors and
#' # `class` is the class outcome/response
#'
#' cells_subset <- modeldata::cells |>
#'   dplyr::select(
#'     class,
#'     angle_ch_1,
#'     area_ch_1,
#'     avg_inten_ch_1,
#'     avg_inten_ch_2,
#'     avg_inten_ch_3
#'   )
#'
#' cells_roc_auc_res <- score_roc_auc |>
#'   fit(class ~ ., data = cells_subset)
#' cells_roc_auc_res@results
#'
#' # ----------------------------------------------------------------------------
#'
#' # ROC AUC where `Sale_Price` is the numeric predictor and the class predictors
#' # are the outcomes/responses
#'
#' ames_subset <- modeldata::ames |>
#'   dplyr::select(
#'     Sale_Price,
#'     MS_SubClass,
#'     MS_Zoning,
#'     Lot_Frontage,
#'     Lot_Area,
#'     Street
#'   )
#' ames_subset <- ames_subset |>
#'   dplyr::mutate(Sale_Price = log10(Sale_Price))
#'
#' ames_roc_auc_res <- score_roc_auc |>
#'   fit(Sale_Price ~ ., data = ames_subset)
#' ames_roc_auc_res@results
#' # TODO Add multiclass example
#' @export
S7::method(fit, class_score_roc_auc) <- function(object, formula, data, ...) {
  analysis_data <- process_all_data(formula, data = data)
  # TODO add case weights

  # Note that model.frame() places the outcome(s) as the first column(s)
  predictors <- names(analysis_data)[-1]
  outcome <- names(analysis_data)[1]

  score <- purrr::map_dbl(
    purrr::set_names(predictors),
    \(x) map_score_roc_auc(analysis_data, predictor = x, outcome = outcome)
  )
  res <- named_vec_to_tibble(score, object@score_type, outcome)

  object@results <- res
  object
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
  return(res)
}
