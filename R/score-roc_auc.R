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
#' The area under the ROC curves can be used to measure predictor importance.
#'
#' @name score_roc_auc
#' @include utilities.R
#' @family class score metrics
#' @details
#' This objects are used when either:
#'
#' - The predictors are numeric and the outcome is a factor/category, or
#' - The predictors are factors and the outcome is numeric.
#'
#' In either case, a ROC curve (via [pROC::roc()] or [pROC::multiclass.roc()]) is created
#' with the proper variable roles, and the area under the ROC curve is computed (via [pROC::auc()]).
#' Values higher than 0.5 (i.e., `max(roc_auc, 1 - roc_auc)` > 0.5) are associated with
#' more important predictors.
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
#'     \item{`case_weights`}{A quantitative vector of case weights that is the same length as the number of rows in `data`. The default of `NULL` indicates that there are no case weights. NOTE case weights cannot be used when a multiclass ROC is computed.}
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
#' - `name`: The name of the score (e.g., `roc_auc`).
#' - `score`: The estimates for each predictor.
#' - `outcome`: The name of the outcome column.
#' - `predictor`: The names of the predictor inputs.
#'
#' These data are accessed using `object@results` (see examples below).
#'
#' @examplesIf rlang::is_installed("modeldata")
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
score_roc_auc <-
  class_score_roc_auc(
    outcome_type = c("numeric", "factor"),
    predictor_type = c("numeric", "factor"),
    case_weights = FALSE, # TODO
    range = c(0, 1),
    inclusive = c(TRUE, TRUE),
    fallback_value = 1,
    score_type = "roc_auc",
    transform_fn = filtro_pmax_trans,
    direction = "maximize",
    deterministic = TRUE,
    tuning = FALSE,
    label = "Area under the Receiver Operating Characteristic curve (ROC AUC)"
  )

# ------------------------------------------------------------------------------

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
    roc <- try(
      pROC::roc(outcome, predictor, direction = "auto", quiet = TRUE, ...),
      silent = TRUE
    )
    if (inherits(roc, "try-error")) {
      roc <- NA_real_
    }
  } else {
    roc <- try(
      pROC::multiclass.roc(
        outcome,
        predictor,
        direction = "auto",
        quiet = TRUE,
        ...
      ),
      silent = TRUE
    )
    if (inherits(roc, "try-error")) {
      roc <- NA_real_
    }
  }
  res <- pROC::auc(roc) |> as.numeric()
  return(res)
}
