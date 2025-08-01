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
    case_weights = FALSE,
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
#'   fit(class ~ ., data = cells_subset, trees = 10)
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
S7::method(fit, class_score_imp_rf) <- function(object, formula, data, ...) {
  analysis_data <- process_all_data(formula, data = data)

  # Note that model.frame() places the outcome(s) as the first column(s)
  predictors <- names(analysis_data)[-1]
  outcome <- names(analysis_data)[1]

  if (object@score_type == "imp_rf") {
    imp <- get_imp_rf_ranger(
      object,
      data = analysis_data,
      outcome = outcome,
      ...
    )
  } else if (object@score_type == "imp_rf_conditional") {
    imp <- get_imp_rf_partykit(
      object,
      data = analysis_data,
      formula = formula,
      ...
    )
  } else if (object@score_type == "imp_rf_oblique") {
    imp <- get_imp_rf_aorsf(
      object,
      data = analysis_data,
      formula = formula,
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

get_imp_rf_ranger <- function(object, data, outcome, ...) {
  if (object@score_type == "imp_rf") {
    importance_type = "permutation"
  } # TODO Allow option for importance = c("impurity")

  y <- data[[outcome]]
  X <- data[setdiff(names(data), outcome)]

  cl <- rlang::call2(
    "ranger",
    .ns = "ranger",
    x = quote(X),
    y = quote(y),
    importance = quote(importance_type)
  )

  # if (!is.null(case_weights)) {
  #   cl <- rlang::call_modify(cl, case.weights = quote(case_weights))
  # }

  opts <- list(...)

  opts <- convert_rf_args(opts, "ranger")
  # Keep consistent with parsnip
  opts <- update_defaults(opts, list(verbose = FALSE, num.threads = 1))

  cl <- rlang::call_modify(cl, !!!opts)

  fit <- rlang::eval_tidy(cl)
  imp <- fit$variable.importance
  imp
}

get_imp_rf_partykit <- function(object, data, formula, ...) {
  cl <- rlang::call2(
    "cforest",
    .ns = "partykit",
    formula = quote(formula),
    data = quote(data)
  )

  # if (!is.null(case_weights)) {
  #   cl <- rlang::call_modify(cl, case.weights = quote(case_weights))
  # }

  opts <- list(...)
  has_control <- any(names(opts) == "control")
  has_min_n <- any(names(opts) == "min_n")

  opts <- convert_rf_args(opts, "partykit")

  if (has_min_n) {
    if (has_control) {
      opts$control$minsplit <- opts$minsplit
    } else {
      opts$control <- partykit::ctree_control(minsplit = opts$minsplit)
    }
    opts$minsplit <- NULL
  }

  cl <- rlang::call_modify(cl, !!!opts)

  fit <- rlang::eval_tidy(cl)

  imp <- partykit::varimp(fit, conditional = TRUE)
  imp
}

get_imp_rf_aorsf <- function(object, data, formula, ...) {
  if (object@score_type == "imp_rf_oblique") {
    importance_type = "permute"
  } # TODO Allow option for importance = c("none", "anova", "negate")

  cl <- rlang::call2(
    "orsf",
    .ns = "aorsf",
    formula = quote(formula),
    data = quote(data),
    importance = quote(importance_type)
  )

  # if (!is.null(case_weights)) {
  #   cl <- rlang::call_modify(cl, case.weights = quote(case_weights))
  # }

  opts <- list(...)

  opts <- convert_rf_args(opts, "aorsf")
  # Keep consistent with parsnip
  opts <- update_defaults(opts, list(verbose_progress = FALSE, n_thread = 1))

  cl <- rlang::call_modify(cl, !!!opts)

  fit <- rlang::eval_tidy(cl)

  imp <- fit$importance
  imp
}

# ------------------------------------------------------------------------------
# Enable users to specific tuning parameters / args with parsnip argument names
# or the original engine names
convert_rf_args <- function(args, method) {
  if (length(args) == 0) {
    return(args)
  }

  tbl <- tibble::as_tibble(args)
  f_args <- names(tbl)
  # get_from_env("rand_forest_args") in parsnip
  # skip: fmt
  arg_data <-
    tibble::tribble(
      ~engine,     ~parsnip,       ~original,
      "ranger",         "mtry",          "mtry",
      "ranger",        "trees",     "num.trees",
      "ranger",        "min_n", "min.node.size",
      "partykit",      "min_n",      "minsplit",
      "partykit",       "mtry",          "mtry",
      "partykit",      "trees",         "ntree",
      "partykit", "tree_depth",      "maxdepth",
      "aorsf",          "mtry",          "mtry",
      "aorsf",         "trees",        "n_tree",
      "aorsf",         "min_n",  "leaf_min_obs"
    ) |>
    dplyr::filter(engine == method & parsnip %in% f_args)

  rnm <- arg_data$parsnip
  names(rnm) <- arg_data$original
  tbl <- dplyr::rename(tbl, !!rnm)
  as.list(tbl)
}

update_defaults <- function(args, defaults = list()) {
  purrr::list_modify(defaults, !!!args)
}
