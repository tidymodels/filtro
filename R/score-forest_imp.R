#' @rdname class_score
#' @include class_score.R
#' @keywords internal
#' @export
class_score_imp_rf <- S7::new_class(
  "class_score_imp_rf",
  parent = class_score,
  properties = list(
    # What is the random forest engine to use for fitting?
    engine = S7::new_property(S7::class_character, default = "ranger"),
    # What is the number of trees contained in the ensemble?
    trees = S7::new_property(S7::class_numeric, default = 100), # TODO May need to set other default
    # What is the number of predictors that will be randomly sampled at each split when creating the tree models?
    mtry = S7::new_property(S7::class_numeric, default = 2),
    # What is the minimum number of data points in a node that are required for the node to be split further?
    min_n = S7::new_property(S7::class_numeric, default = 1),
    # What is the task type? Relevant only for ranger.
    mode = S7::new_property(S7::class_character, default = "classification"), # TODO True, False?
    # What is the random seed?
    seed = S7::new_property(S7::class_numeric, default = 42)
  )
)

#' Scoring via random forests
#'
#' @description
#'
#' These objects are used when either:
#'
#' - The predictors are numeric and the outcome is a factor/category, or
#' - The predictors are factors and the outcome is numeric.
#'
#' In either case, a random forest, conditional random forest, or oblique random forest
#' (via [ranger::ranger()], [partykit::cforest()], or [aorsf::orsf()]) is created with
#' the proper variable roles, and the feature importance scores are computed. Larger
#' values are associated with more important predictors.
#'
#' `score_imp_rf`, `score_imp_rf_conditional` and `score_imp_rf_oblique` are
#' objects that define the technique.
#' To apply the filter on data, you would use the [fit()] method:
#'
#' \preformatted{
#'   fit(score_imp_rf, formula, data)
#' }
#'
#' See the Examples section below.
#' @name score_imp_rf
#' @export
score_imp_rf <-
  class_score_imp_rf(
    outcome_type = c("numeric", "factor"),
    predictor_type = c("numeric", "factor"),
    case_weights = TRUE,
    range = c(0, Inf),
    inclusive = c(FALSE, FALSE),
    fallback_value = Inf,
    score_type = "imp_rf",
    direction = "maximize",
    deterministic = FALSE,
    tuning = TRUE,
    label = "Random forest importance scores"
  )

#' @name score_imp_rf
#' @export
score_imp_rf_conditional <-
  class_score_imp_rf(
    outcome_type = c("numeric", "factor"),
    predictor_type = c("numeric", "factor"),
    case_weights = FALSE,
    range = c(0, Inf),
    inclusive = c(FALSE, FALSE),
    fallback_value = Inf,
    score_type = "imp_rf_conditional",
    direction = "maximize",
    deterministic = FALSE,
    tuning = TRUE,
    label = "Random forest importance scores",
    engine = "partykit"
  )

#' @name score_imp_rf
#' @export
score_imp_rf_oblique <-
  class_score_imp_rf(
    outcome_type = c("numeric", "factor"),
    predictor_type = c("numeric", "factor"),
    case_weights = FALSE,
    range = c(0, Inf),
    inclusive = c(FALSE, FALSE),
    fallback_value = Inf,
    score_type = "imp_rf_oblique",
    direction = "maximize",
    deterministic = FALSE,
    tuning = TRUE,
    label = "Random forest importance scores",
    engine = "aorsf"
  )

# ------------------------------------------------------------------------------

#' Compute random forest feature importance scores
#' @name score_imp_rf
#' @include class_score.R
#' @param object A score class object based on `class_score_imp_rf`.
#' @param formula A standard R formula with a single outcome on the right-hand
#' side and one or more predictors (or `.`) on the left-hand side. The data are
#' processed via [stats::model.frame()].
#' @param data A data frame containing the relevant columns defined by the
#' formula.
#' @param case_weights A quantitative vector of case weights that is the same
#' length as the number of rows in `data`. The default of `NULL` indicates that
#' there are no case weights.
#' @param ... Further arguments passed to or from other methods.
#' @details
#' The function will determine which columns are predictors and outcomes in the
#' random forest; no user intervention is required.
#'
#' Missing values are removed for each predictor/outcome combination being
#' scored.
#' When a predictor's importance score is 0, [partykit::cforest()] may omit its
#' name from the results. In cases like these, a score of 0 is assigned to the
#' missing predictors.
#'
#' @examplesIf rlang::is_installed("modeldata")
#'
#' library(dplyr)
#'
#' # Random forests for classification task
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
#' # Random forest
#' set.seed(42)
#' cells_imp_rf_res <- score_imp_rf |>
#'   fit(class ~ ., data = cells_subset)
#' cells_imp_rf_res@results
#'
#' # Conditional random forest
#' cells_imp_rf_conditional_res <- score_imp_rf_conditional |>
#'   fit(class ~ ., data = cells_subset)
#' cells_imp_rf_conditional_res@results
#'
#' # Oblique random forest
#' cells_imp_rf_oblique_res <- score_imp_rf_oblique |>
#'   fit(class ~ ., data = cells_subset)
#' cells_imp_rf_oblique_res@results
#'
#' # ----------------------------------------------------------------------------
#'
#' # Random forests for regression task
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
#' regression_task <- score_imp_rf
#' regression_task@mode <- "regression"
#'
#' set.seed(42)
#' ames_imp_rf_regression_task_res <-
#'   regression_task |>
#'   fit(Sale_Price ~ ., data = ames_subset)
#' ames_imp_rf_regression_task_res@results
#' # TODO Add example of how to change trees, mtry, min_n, seed
#' @export
S7::method(fit, class_score_imp_rf) <- function(
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

  complete_obs <- !is.na(analysis_data[outcome])
  if (!is.null(case_weights)) {
    complete_obs <- complete_obs & !is.na(case_weights)
  }
  analysis_data <- analysis_data[complete_obs, ]

  if (object@score_type == "imp_rf") {
    imp <- get_imp_rf_ranger(
      object,
      data = analysis_data,
      outcome = outcome,
      weights = case_weights,
      ...
    )
  } else if (object@score_type == "imp_rf_conditional") {
    imp <- get_imp_rf_partykit(
      object,
      data = analysis_data,
      formula = formula,
      weights = case_weights,
      ...
    )
  } else if (object@score_type == "imp_rf_oblique") {
    imp <- get_imp_rf_aorsf(
      object,
      data = analysis_data,
      formula = formula,
      weights = case_weights,
      ...
    )
  }

  score <- imp[predictors]
  score[is.na(score)] <- 0
  score <- stats::setNames(score, nm = predictors) # TODO Confirm this is the right approach

  res <- named_vec_to_tibble(score, object@score_type, outcome)

  object@results <- res
  object
}

get_imp_rf_ranger <- function(object, data, outcome, weights, ...) {
  if (object@score_type == "imp_rf") {
    importance_type = "permutation"
  } # TODO Allow option for importance = c("impurity")

  y <- data[[outcome]]
  X <- data[setdiff(names(data), outcome)]

  complete_obs <- complete.cases(X) & !is.na(y)
  y <- y[complete_obs]
  X <- X[complete_obs, , drop = FALSE]

  if (!is.null(weights)) {
    weights <- weights[complete_obs]
  } else {
    weights <- rep(1.0, length(y))
  }

  cl <- rlang::call2(
    "ranger",
    .ns = "ranger",
    x = quote(X),
    y = quote(y),
    importance = quote(importance_type),
    classification = object@mode == "classification",
    case.weights = quote(weights)
  )

  # if (!is.null(case_weights)) {
  #   cl <- rlang::call_modify(cl, case.weights = quote(case_weights))
  # }

  opts <- list(...)

  if (is.null(opts[["trees"]])) {
    opts$trees <- object@trees
  }
  if (is.null(opts[["mtry"]])) {
    opts$mtry <- object@mtry
  }
  if (is.null(opts[["min_n"]])) {
    opts$min_n <- object@min_n
  }

  if ("trees" %in% names(opts)) {
    opts[["num.trees"]] <- opts[["trees"]]
    opts[["trees"]] <- NULL
  }

  if ("min_n" %in% names(opts)) {
    opts[["min.node.size"]] <- opts[["min_n"]]
    opts[["min_n"]] <- NULL
  }

  cl <- rlang::call_modify(cl, !!!opts)

  fit <- rlang::eval_tidy(cl)
  imp <- fit$variable.importance
  imp
}

get_imp_rf_partykit <- function(object, data, formula, weights, ...) {
  complete_obs <- stats::complete.cases(data)
  data <- data[complete_obs, , drop = FALSE]

  if (!is.null(weights)) {
    weights <- weights[complete_obs]
  } else {
    weights <- rep(1.0, nrow(data))
  }

  cl <- rlang::call2(
    "cforest",
    .ns = "partykit",
    formula = quote(formula), # Do we want rlang::expr() instead?
    data = quote(data),
    weights = quote(weights)
  )

  # if (!is.null(case_weights)) {
  #   cl <- rlang::call_modify(cl, case.weights = quote(case_weights))
  # }

  opts <- list(...)

  if (is.null(opts[["trees"]])) {
    opts$trees <- object@trees
  }
  if (is.null(opts[["mtry"]])) {
    opts$mtry <- object@mtry
  }
  if (is.null(opts[["min_n"]])) {
    opts$min_n <- object@min_n
  }

  if ("trees" %in% names(opts)) {
    opts[["ntree"]] <- opts[["trees"]]
    opts[["trees"]] <- NULL
  }

  if ("min_n" %in% names(opts)) {
    # TODO Check parsnip's partykit.R
    opts[["control"]] <- partykit::ctree_control(minsplit = opts[["min_n"]])
    opts[["min_n"]] <- NULL
  }

  cl <- rlang::call_modify(cl, !!!opts)

  fit <- rlang::eval_tidy(cl)
  imp <- partykit::varimp(fit, conditional = TRUE)
  imp
}

get_imp_rf_aorsf <- function(object, data, formula, weights, ...) {
  if (object@score_type == "imp_rf_oblique") {
    importance_type = "permute"
  } # TODO Allow option for importance = c("none", "anova", "negate")

  complete_obs <- stats::complete.cases(data)
  data <- data[complete_obs, , drop = FALSE]

  if (!is.null(weights)) {
    weights <- weights[complete_obs]
  } else {
    weights <- rep(1.0, nrow(data))
  }

  cl <- rlang::call2(
    "orsf",
    .ns = "aorsf",
    formula = quote(formula),
    data = quote(data),
    importance = quote(importance_type),
    weights = quote(weights)
  )

  # if (!is.null(case_weights)) {
  #   cl <- rlang::call_modify(cl, case.weights = quote(case_weights))
  # }

  opts <- list(...)

  if (is.null(opts[["trees"]])) {
    opts$trees <- object@trees
  }
  if (is.null(opts[["mtry"]])) {
    opts$mtry <- object@mtry
  }

  if ("trees" %in% names(opts)) {
    opts[["n_tree"]] <- opts[["trees"]]
    opts[["trees"]] <- NULL
  }

  if ("mtry" %in% names(opts)) {
    opts[["n_retry"]] <- opts[["mtry"]]
    opts[["mtry"]] <- NULL
  }

  cl <- rlang::call_modify(cl, !!!opts)

  fit <- rlang::eval_tidy(cl)
  imp <- fit$importance
  imp
}
