#' Construct a subclassed score object for feature importance scores with additional metadata
#'
#' Introduce new properties `engine`, `trees`, `mtry`, `min_n` and `is_reg`.
#' Output a new score object that contains associated metadata, such as `range`,
#' `fallback_value`, `score_type`, `direction`, and other relevant attributes.
#'
#' @inheritParams new_score_obj
#'
#' @param fallback_value A numeric scalar used as a fallback value. One of:
#'   - `Inf` (default)
#' @param score_type A character string indicating the type of scoring metric to compute.
#' One of:
#'    - `"imp_rf"` (default)
#'    - `"imp_rf_conditional"`
#'    - `"imp_rf_oblique"`
#' @param direction A character string indicating the optimization direction. One of:
#'  - `"maximize"` (default)
#'  - `"minimize"`
#'  - `"target"`
#' @param engine A character string specifying the random forest engine to use for fitting. One of:
#'  - `"ranger"` (default)
#'  - `"partykit"`
#'  - `"aorsf"`
#' @param trees An integer for the number of trees contained in the ensemble.
#' @param mtry An integer for the number of predictors that will
#' be randomly sampled at each split when creating the tree models.
#' @param min_n An integer for the minimum number of data points
#' in a node that are required for the node to be split further.
#' @param mode A character string indicating the task type. One of:
#'  - `"regression"`
#'  - `"classification"`
#'
#' @returns A score object containing associated metadata such as `range`, `fallback_value`,
#' `score_type`, `direction`, and other relevant attributes.
#' @export
#'
#' @examples
#' # Create a score object
#' new_score_obj_forest_imp()
new_score_obj_forest_imp <- S7::new_class(
  "new_score_obj_forest_imp",
  parent = new_score_obj,
  properties = list(
    engine = S7::new_property(S7::class_character, default = "ranger"),
    trees = S7::new_property(S7::class_numeric, default = 10), # TODO May need to set other default
    mtry = S7::new_property(S7::class_numeric, default = 2),
    min_n = S7::new_property(S7::class_numeric, default = 1),
    mode = S7::new_property(S7::class_character, default = "classification")
  )
)

#' Create a score object for feature importance scores
#'
#' Construct a score object containing metadata for feature scoring using a
#' random forest, a conditional random forest or an oblique random forest
#' Output a score object containing associated metadata such as `range`, `fallback_value`,
#' `score_type` (`"imp_rf"`, `"imp_rf_conditional"`, `"imp_rf_oblique"`), `direction`, and other relevant attributes.
#'
#' @inheritParams new_score_obj_forest_imp
#'
#' @returns A score object containing associated metadata such as `range`, `fallback_value`,
#' `score_type` (`"imp_rf"`, `"imp_rf_conditional"`, `"imp_rf_oblique"`), `direction`, and other relevant attributes.
#'
#' @export
#'
#' @examples NULL
score_forest_imp <- function(
  range = c(0, Inf),
  fallback_value = Inf,
  score_type = "imp_rf",
  direction = "maximize",
  engine = "ranger",
  trees = 10,
  mtry = 2,
  min_n = 1,
  mode = "classification"
) {
  new_score_obj_forest_imp(
    outcome_type = "numeric",
    predictor_type = "numeric",
    case_weights = FALSE,
    range = range,
    inclusive = c(TRUE, TRUE),
    fallback_value = fallback_value,
    score_type = score_type,
    #trans = # Cannot set NULL. Otherwise S7 complains
    #sorts =
    direction = direction,
    deterministic = FALSE,
    tuning = TRUE,
    #ties =
    calculating_fn = function(x) {}, # Otherwise S7 complains
    engine = engine,
    trees = trees,
    mtry = mtry,
    min_n = min_n,
    mode = mode,
    label = c(score_rfimp = "Random Forest importance scores")
  )
}

get_imp_rf_ranger <- function(score_obj, data, outcome) {
  if (score_obj@score_type == "imp_rf") {
    importance_type = "permutation"
  } # TODO Allow option for importance = c("impurity")

  y <- data[[outcome]]
  X <- data[setdiff(names(data), outcome)]
  fit <- ranger::ranger(
    x = X,
    y = y,
    num.trees = score_obj@trees,
    mtry = score_obj@mtry,
    importance = importance_type,
    min.node.size = score_obj@min_n,
    classification = score_obj@mode == "classification",
    seed = 42 # TODO Add this to pass tests. Remove later.
  )
  imp <- fit$variable.importance
  imp
}

get_imp_rf_partykit <- function(score_obj, data, formula) {
  fit <- partykit::cforest(
    formula = formula,
    data = data,
    control = partykit::ctree_control(minsplit = score_obj@min_n), # TODO Eventually have user pass in ctree_control()
    ntree = score_obj@trees,
    mtry = score_obj@mtry,
  )
  imp <- partykit::varimp(fit, conditional = TRUE)
  imp
}

get_imp_rf_aorsf <- function(score_obj, data, formula) {
  if (score_obj@score_type == "imp_rf_oblique") {
    importance_type = "permute"
  } # TODO Allow option for importance = c("none", "anova", "negate")

  fit <- aorsf::orsf(
    formula = formula,
    data = data,
    n_tree = score_obj@trees,
    n_retry = score_obj@mtry,
    importance = importance_type
  )
  imp <- fit$importance # orsf_vi_permute(fit)
  imp
}

make_scores_forest_importance <- function(
  score_type,
  imp,
  outcome,
  predictors
) {
  score <- imp[predictors] |> unname()
  score[is.na(score)] <- 0

  res <- tibble::tibble(
    name = score_type,
    score = score,
    outcome = outcome,
    predictor = predictors
  )
  res
}

#' Compute feature importance scores using a random forest, a conditional random forest, or
#' an oblique random forest
#'
#' Evaluate the relationship between a numeric outcome and a categorical predictor,
#' or vice versa, by computing feature importance scores.
#' Output a tibble result with with one row per predictor, and four columns:
#' `name`, `score`, `predictor`, and `outcome`.
#'
#' @param score_obj A score object. See [score_forest_imp()] for details.
#'
#' @param data A data frame or tibble containing the outcome and predictor variables.
#' @param outcome A character string specifying the name of the outcome variable.
#' @param ... NULL
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
#' # Return importance score using ranger for classification task
#' cells_subset <- modeldata::cells |>
#'   dplyr::select(
#'     class,
#'     angle_ch_1,
#'     area_ch_1,
#'     avg_inten_ch_1,
#'     avg_inten_ch_2
#'   )
#' score_obj <- score_forest_imp()
#' score_res <- get_scores_forest_importance(
#'   score_obj,
#'   data = cells_subset,
#'   outcome = "class"
#')
#' # Return importance score using partykit
#' score_obj <- score_forest_imp(engine = "partykit")
#' score_res <- get_scores_forest_importance(
#'   score_obj,
#'   data = cells_subset,
#'   outcome = "class"
#' )
#' # Return importance score using aorsf
#' score_obj <- score_forest_imp(engine = "aorsf")
#' score_res <- get_scores_forest_importance(
#'   score_obj,
#'   data = cells_subset,
#'   outcome = "class"
#' )
#' # Return importance score using ranger for regression task
#' data(ames, package = "modeldata")
#' ames_subset <- modeldata::ames |>
#'   dplyr::select(
#'     Sale_Price,
#'     MS_SubClass,
#'     MS_Zoning,
#'     Lot_Frontage,
#'     Lot_Area,
#'     Street
#'   )
#' score_obj <- score_forest_imp(is_reg = TRUE)
#' score_res <- get_scores_forest_importance(
#'   score_obj,
#'   data = ames_subset,
#'   outcome = "Sale_Price"
#' )
get_scores_forest_importance <- function(
  score_obj,
  data,
  outcome,
  ... # i.e., score_obj$engine, score_obj$trees, score_obj$mtry, score_obj$min_n, score_obj$is_reg
) {
  outcome_name <- outcome |> as.name()
  formula <- stats::as.formula(paste(outcome_name, "~ ."))
  predictors <- setdiff(names(data), outcome)

  if (score_obj@engine == "ranger") {
    score_obj@score_type <- "imp_rf"
    imp <- get_imp_rf_ranger(score_obj, data, outcome)
  } else if (score_obj@engine == "partykit") {
    score_obj@score_type <- "imp_rf_conditional"
    imp <- get_imp_rf_partykit(score_obj, data, formula)
  } else if (score_obj@engine == "aorsf") {
    score_obj@score_type <- "imp_rf_oblique"
    imp <- get_imp_rf_aorsf(score_obj, data, formula)
  }
  res <- make_scores_forest_importance(
    score_obj@score_type,
    imp,
    outcome,
    predictors
  )
  res
}
