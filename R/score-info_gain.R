#' @rdname class_score
#' @include class_score.R
#' @keywords internal
#' @export
class_score_info_gain <- S7::new_class(
  "class_score_info_gain",
  parent = class_score,
  properties = list(
    # What is the task type?
    mode = S7::new_property(S7::class_character, default = "classification") # TODO True, False?
  )
)

#' Scoring via entropy-based filters
#'
#' @description
#'
#' These objects are used when either:
#'
#' - The predictors are numeric and the outcome is a factor/category, or
#' - The predictors are factors and the outcome is numeric.
#'
#' In either case, an entropy-based filter (via [FSelectorRcpp::information_gain()])
#' is applied with the proper variable roles. Depending on the chosen method, information
#' gain, gain ratio, or symmetrical uncertainty is computed. Larger values are associated
#' with more important predictors.
#'
#' `score_info_gain`, `score_gain_ratio` and `score_sym_uncert` are
#' objects that define the technique.
#' To apply the filter on data, you would use the [fit()] method:
#'
#' \preformatted{
#'   fit(score_info_gain, formula, data)
#' }
#'
#' See the Examples section below.
#' @name score_info_gain
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
    direction = "maximize",
    deterministic = TRUE,
    tuning = FALSE,
    label = "Entropy-based symmetrical uncertainty"
  )

# ------------------------------------------------------------------------------

#' Compute entropy-based filter information gain
#' @name score_info_gain
#' @include class_score.R
#' @param object A score class object based on `score_info_gain`.
#' @param formula A standard R formula with a single outcome on the right-hand
#' side and one or more predictors (or `.`) on the left-hand side. The data are
#' processed via [stats::model.frame()].
#' @param data A data frame containing the relevant columns defined by the
#' formula.
#' @param ... Further arguments passed to or from other methods.
#' @details
#' The function will determine which columns are predictors and outcomes in the
#' filter; no user intervention is required.
#'
#' Missing values are removed for each predictor/outcome combination being
#' scored.
#' When a predictor's importance score is 0, the gain ratio method may omit its
#' name from the results. In cases like these, a score of 0 is assigned to the
#' missing predictors.
#'
#' @examples
#' if (rlang::is_installed("modeldata")) {
#'
#'   library(dplyr)
#'
#'   # Entropy-based filter for classification tasks
#'
#'   cells_subset <- modeldata::cells |>
#'     dplyr::select(
#'       class,
#'       angle_ch_1,
#'       area_ch_1,
#'       avg_inten_ch_1,
#'       avg_inten_ch_2,
#'       avg_inten_ch_3
#'     )
#'
#'   cells_info_gain_res <- score_info_gain |>
#'     fit(class ~ ., data = cells_subset)
#'   cells_info_gain_res@results
#'
#'   cells_gain_ratio_res <- score_gain_ratio |>
#'     fit(class ~ ., data = cells_subset)
#'   cells_gain_ratio_res@results
#'
#'   cells_sym_uncert_res <- score_sym_uncert |>
#'     fit(class ~ ., data = cells_subset)
#'   cells_sym_uncert_res@results
#'
#'   # ----------------------------------------------------------------------------
#'
#'   # Entropy-based filter for regression tasks
#'
#'   ames_subset <- modeldata::ames |>
#'     dplyr::select(
#'       Sale_Price,
#'       MS_SubClass,
#'       MS_Zoning,
#'       Lot_Frontage,
#'       Lot_Area,
#'       Street
#'     )
#'   ames_subset <- ames_subset |>
#'     dplyr::mutate(Sale_Price = log10(Sale_Price))
#'
#'   regression_task <- score_info_gain
#'   regression_task@mode <- "regression"
#'
#'   ames_info_gain_regression_task_res <-
#'     regression_task |>
#'     fit(Sale_Price ~ ., data = ames_subset)
#'   ames_info_gain_regression_task_res@results
#' }
#' @export
S7::method(fit, class_score_info_gain) <- function(object, formula, data, ...) {
  analysis_data <- process_all_data(formula, data = data)

  # Note that model.frame() places the outcome(s) as the first column(s)
  predictors <- names(analysis_data)[-1]
  outcome <- names(analysis_data)[1]

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
  imp <- FSelectorRcpp::information_gain(
    x = X,
    y = y,
    type = object@score_type,
    equal = object@mode == "regression" # Set = TRUE for numeric outcome
  )
}
