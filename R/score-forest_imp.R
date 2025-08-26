#' @rdname class_score
#' @include class_score.R
#' @keywords internal
#' @export
class_score_imp_rf <- S7::new_class(
  "class_score_imp_rf",
  parent = class_score,
  properties = list(
    # What is the random forest engine to use for fitting?
    engine = S7::new_property(S7::class_character, default = "ranger")
  )
)

#' Scoring via random forests
#'
#' @description
#'
#' Three different random forest models can be used to measure predictor importance.
#'
#' @name score_imp_rf
#' @aliases score_imp_rf_conditional score_imp_rf_oblique
#' @family class score metrics
#'
#' @details
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
#' When a predictor's importance score is 0, [partykit::cforest()] may omit its
#' name from the results. In cases like these, a score of 0 is assigned to the
#' missing predictors.
#'
#' ## Estimating the scores
#'
#' In \pkg{filtro}, the `score_*` objects define a scoring method (e.g., data
#' input requirements, package dependencies, etc). To compute the scores for
#' a specific data set, the `fit()` method is used. The main arguments for
#' these functions are:
#'
#'   \describe{
#'     \item{`object`}{A score class object (e.g., `score_imp_rf`).}
#'     \item{`formula`}{A standard R formula with a single outcome on the right-hand side and one or more predictors (or `.`) on the left-hand side. The data are processed via [stats::model.frame()]}
#'     \item{`data`}{A data frame containing the relevant columns defined by the formula.}
#'     \item{`...`}{Further arguments passed to or from other methods.}
#'     \item{`case_weights`}{A quantitative vector of case weights that is the same length as the number of rows in `data`. The default of `NULL` indicates that there are no case weights.}
#'   }
#'
#' Missing values are removed by case-wise deletion.
#'
#' @includeRmd man/rmd/fault_tolerant.Rmd details
#'
#' @return An S7 object. The primary property of interest is in `results`. This
#' is a data frame of results that is populated by the `fit()` method and has
#' columns:
#'
#' - `name`: The name of the score (e.g., `imp_rf`).
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
#' # Random forests for classification task
#'
#' cells_subset <- modeldata::cells |>
#'   # Use a small example for efficiency
#'   dplyr::select(
#'     class,
#'     angle_ch_1,
#'     area_ch_1,
#'     avg_inten_ch_1,
#'     avg_inten_ch_2,
#'     avg_inten_ch_3
#'   ) |>
#'   slice(1:50)
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
#'   # Use a small example for efficiency
#'   dplyr::select(
#'     Sale_Price,
#'     MS_SubClass,
#'     MS_Zoning,
#'     Lot_Frontage,
#'     Lot_Area,
#'     Street
#'   ) |>
#'   slice(1:50)
#' ames_subset <- ames_subset |>
#'   dplyr::mutate(Sale_Price = log10(Sale_Price))
#'
#' set.seed(42)
#' ames_imp_rf_regression_task_res <-
#'   score_imp_rf |>
#'   fit(Sale_Price ~ ., data = ames_subset)
#' ames_imp_rf_regression_task_res@results
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
    transform_fn = function(x) x,
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
    transform_fn = function(x) x,
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
    transform_fn = function(x) x,
    direction = "maximize",
    deterministic = FALSE,
    tuning = TRUE,
    label = "Random forest importance scores",
    engine = "aorsf"
  )

# ------------------------------------------------------------------------------

#' @export
S7::method(fit, class_score_imp_rf) <- function(
  object,
  formula,
  data,
  case_weights = NULL,
  ...
) {
  analysis_data <- process_all_data(formula, data = data)

  # check for zero-variance columns and remove them
  zv_names <- find_zero_variance_cols(analysis_data)
  if (length(zv_names) > 0) {
    analysis_data <- dplyr::select(analysis_data, -all_of(!!zv_names))
    zv_vec <- rep(0.0, length(zv_names))
    names(zv_vec) <- zv_names
  } else {
    zv_vec <- double(0)
  }

  predictors <- names(analysis_data)[-1]
  outcome <- names(analysis_data)[1]
  case_weights <- convert_weights(case_weights, nrow(analysis_data))

  complete_obs <- !is.na(analysis_data[outcome])
  if (!is.null(case_weights)) {
    complete_obs <- complete_obs & !is.na(case_weights)
  }
  analysis_data <- analysis_data[complete_obs, ]

  if (object@score_type == "imp_rf") {
    imp <- try(
      get_imp_rf_ranger(
        object,
        data = analysis_data,
        outcome = outcome,
        weights = case_weights,
        ...
      ),
      silent = TRUE
    )
  } else if (object@score_type == "imp_rf_conditional") {
    imp <- try(
      get_imp_rf_partykit(
        object,
        data = analysis_data,
        formula = formula,
        weights = case_weights,
        ...
      ),
      silent = TRUE
    )
  } else if (object@score_type == "imp_rf_oblique") {
    imp <- try(
      get_imp_rf_aorsf(
        object,
        data = analysis_data,
        formula = formula,
        weights = case_weights,
        ...
      ),
      silent = TRUE
    )
  }

  if (inherits(imp, "try-error")) {
    cli::cli_warn("Random forest importance calucations errored with: {imp}")
    imp <- rep(NA_real_, length(predictors))
  }

  imp <- c(imp, zv_vec)

  res <- named_vec_to_tibble(imp, object@score_type, outcome)

  object@results <- res
  object
}

get_imp_rf_ranger <- function(object, data, outcome, weights, ...) {
  if (object@score_type == "imp_rf") {
    importance_type = "permutation"
  } # TODO Allow option for importance = c("impurity")

  y <- data[[outcome]]
  X <- data[setdiff(names(data), outcome)]

  complete_obs <- stats::complete.cases(X) & !is.na(y)
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
    importance = quote(importance_type)
  )

  opts <- list(...)

  opts <- convert_rf_args(opts, "ranger")
  # Keep consistent with parsnip
  opts <- update_defaults(
    opts,
    list(verbose = FALSE, num.threads = 1, seed = sample.int(1000, 1))
  )
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
      ~engine,
      ~parsnip,
      ~original,
      "ranger",
      "mtry",
      "mtry",
      "ranger",
      "trees",
      "num.trees",
      "ranger",
      "min_n",
      "min.node.size",
      "partykit",
      "min_n",
      "minsplit",
      "partykit",
      "mtry",
      "mtry",
      "partykit",
      "trees",
      "ntree",
      "partykit",
      "tree_depth",
      "maxdepth",
      "aorsf",
      "mtry",
      "mtry",
      "aorsf",
      "trees",
      "n_tree",
      "aorsf",
      "min_n",
      "leaf_min_obs"
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
