#' @rdname class_score
#' @include class_score.R
#' @keywords internal
#' @export
class_score_info_gain <- S7::new_class(
  "class_score_info_gain",
  parent = class_score,
  properties = list(
    # What is the task type?
    mode = S7::new_property(S7::class_character, default = "classification")
  )
)

#' Scoring via entropy-based filters
#'
#' @description
#'
#' Three different information theory (entropy) scores can be computed.
#'
#' @name score_info_gain
#' @family class score metrics
#'
#' @details
#'
#' These objects are used when either:
#'
#' - The predictors are numeric and the outcome is a factor/category, or
#' - The predictors are factors and the outcome is numeric.
#'
#' In either case, an entropy-based filter (via
#' [FSelectorRcpp::information_gain()]) is applied with the proper variable
#' roles. Depending on the chosen method, information gain, gain ratio, or
#' symmetrical uncertainty is computed. Larger values are associated with more
#' important predictors.
#'
#' ## Estimating the scores
#'
#' In \pkg{filtro}, the `score_*` objects define a scoring method (e.g., data
#' input requirements, package dependencies, etc). To compute the scores for
#' a specific data set, the `fit()` method is used. The main arguments for
#' these functions are:
#'
#'   \describe{
#'     \item{`object`}{A score class object (e.g., `score_info_gain`).}
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
#' - `name`: The name of the score (e.g., `info_gain`).
#' - `score`: The estimates for each predictor.
#' - `outcome`: The name of the outcome column.
#' - `predictor`: The names of the predictor inputs.
#'
#' These data are accessed using `object@results` (see examples below).
#'
#' @examplesIf rlang::is_installed("modeldata")
#'
#' library(dplyr)
#'
#' # Entropy-based filter for classification tasks
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
#' # Information gain
#' cells_info_gain_res <- score_info_gain |>
#'   fit(class ~ ., data = cells_subset)
#' cells_info_gain_res@results
#'
#' # Gain ratio
#' cells_gain_ratio_res <- score_gain_ratio |>
#'   fit(class ~ ., data = cells_subset)
#' cells_gain_ratio_res@results
#'
#' # Symmetrical uncertainty
#' cells_sym_uncert_res <- score_sym_uncert |>
#'   fit(class ~ ., data = cells_subset)
#' cells_sym_uncert_res@results
#'
#' # ----------------------------------------------------------------------------
#'
#' # Entropy-based filter for regression tasks
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
#' regression_task <- score_info_gain
#' regression_task@mode <- "regression"
#'
#' ames_info_gain_regression_task_res <-
#'   regression_task |>
#'   fit(Sale_Price ~ ., data = ames_subset)
#' ames_info_gain_regression_task_res@results
#' @export
score_info_gain <-
  class_score_info_gain(
    outcome_type = c("numeric", "factor"),
    predictor_type = c("numeric", "factor"),
    case_weights = FALSE,
    range = c(0, Inf),
    inclusive = c(FALSE, FALSE),
    fallback_value = Inf,
    score_type = "infogain",
    transform_fn = function(x) x,
    direction = "maximize",
    deterministic = TRUE,
    tuning = FALSE,
    label = "Entropy-based information gain"
  )

#' @name score_info_gain
#' @export
score_gain_ratio <-
  class_score_info_gain(
    outcome_type = c("numeric", "factor"),
    predictor_type = c("numeric", "factor"),
    case_weights = FALSE,
    range = c(0, 1),
    inclusive = c(TRUE, TRUE),
    fallback_value = 1,
    score_type = "gainratio",
    transform_fn = function(x) x,
    direction = "maximize",
    deterministic = TRUE,
    tuning = FALSE,
    label = "Entropy-based gain ratio"
  )

#' @name score_info_gain
#' @export
score_sym_uncert <-
  class_score_info_gain(
    outcome_type = c("numeric", "factor"),
    predictor_type = c("numeric", "factor"),
    case_weights = FALSE,
    range = c(0, 1),
    inclusive = c(TRUE, TRUE),
    fallback_value = 1,
    score_type = "symuncert",
    transform_fn = function(x) x,
    direction = "maximize",
    deterministic = TRUE,
    tuning = FALSE,
    label = "Entropy-based symmetrical uncertainty"
  )

# ------------------------------------------------------------------------------

#' @export
S7::method(fit, class_score_info_gain) <- function(object, formula, data, ...) {
  analysis_data <- process_all_data(formula, data = data)

  # Note that model.frame() places the outcome(s) as the first column(s)
  predictors <- names(analysis_data)[-1]
  outcome <- names(analysis_data)[1]

  if (is.numeric(analysis_data[[outcome]])) {
    object@mode <- "regression"
  }

  imp <- get_info_gain(object, data = analysis_data, outcome = outcome)

  score <- imp$importance
  score[is.na(score)] <- 0
  score <- stats::setNames(score, nm = predictors) # TODO Confirm this is the right approach

  res <- named_vec_to_tibble(score, object@score_type, outcome)

  object@results <- res
  object
}

get_info_gain <- function(object, data, outcome) {
  y <- data[[outcome]]
  X <- data[setdiff(names(data), outcome)]
  imp <- try(
    FSelectorRcpp::information_gain(
      x = X,
      y = y,
      type = object@score_type,
      equal = object@mode == "regression" # Set = TRUE for numeric outcome
    ),
    silent = TRUE
  )
  if (inherits(imp, "try-error")) {
    imp <- NA_real_
  }
  return(imp)
}
