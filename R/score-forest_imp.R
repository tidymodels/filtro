#' Compute feature importance scores using a random forest, a conditional random forest, or
#' an oblique random forest
#'
#' @param range NULL
#' @param trans NULL
#' @param score_type NULL
#' @param direction NULL
#'
#' @returns NULL
#' @export
#'
#' @examples NULL
score_forest_imp <- function(
  range = c(0, Inf),
  trans = NULL,
  score_type = "imp_rf", # Move c() here later. Add validator. Document it.
  direction = "maximize"
) {
  new_score_obj(
    subclass = c("num_num"),
    outcome_type = "numeric",
    predictor_type = "numeric",
    case_weights = FALSE, # TODO
    range = range,
    inclusive = c(TRUE, TRUE),
    fallback_value = Inf,
    score_type = score_type, # c("imp_rf", "imp_rf_conditional", "imp_rf_oblique")
    trans = NULL, # TODO
    sorts = NULL, # TODO
    direction = c("maximize", "minimize", "target"),
    deterministic = FALSE,
    tuning = TRUE,
    ties = NULL,
    calculating_fn = NULL,
    label = c(score_rfimp = "Random Forest importance scores")
  )
}

get_imp_rf_ranger <- function(score_obj, data, outcome) {
  if (score_obj$score_type == "imp_rf") {
    importance_type = "permutation"
  } # TODO Allow option for importance = c("impurity")

  y <- data[[outcome]]
  X <- data[setdiff(names(data), outcome)]
  fit <- ranger::ranger(
    x = X,
    y = y,
    num.trees = score_obj$trees,
    mtry = score_obj$mtry,
    importance = importance_type,
    min.node.size = score_obj$min_n,
    classification = score_obj$class, # TODO There is probably a better way to to do this?
    seed = 42 # TODO Add this to pass tests. Remove later.
  )
  imp <- fit$variable.importance
  imp
}

get_imp_rf_partykit <- function(score_obj, data, formula) {
  fit <- partykit::cforest(
    formula = formula,
    data = data,
    control = partykit::ctree_control(minsplit = score_obj$min_n), # TODO Eventually have user pass in ctree_control()
    ntree = score_obj$trees,
    mtry = score_obj$mtry,
  )
  imp <- partykit::varimp(fit, conditional = TRUE)
  imp
}

get_imp_rf_aorsf <- function(score_obj, data, formula) {
  if (score_obj$score_type == "imp_rf_oblique") {
    importance_type = "permute"
  } # TODO Allow option for importance = c("none", "anova", "negate")

  fit <- aorsf::orsf(
    formula = formula,
    data = data,
    n_tree = score_obj$trees,
    n_retry = score_obj$mtry,
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

  res <- dplyr::tibble(
    name = score_type,
    score = score,
    outcome = outcome,
    predictor = predictors
  )
  res
}

get_scores_forest_importance <- function(
  score_obj,
  data,
  outcome,
  ... # i.e., score_obj$engine, score_obj$trees, score_obj$mtry, score_obj$min_n
) {
  outcome_name <- outcome |> as.name()
  formula <- stats::as.formula(paste(outcome_name, "~ ."))
  predictors <- setdiff(names(data), outcome)

  if (score_obj$engine == "ranger") {
    score_obj$score_type <- "imp_rf"
    imp <- get_imp_rf_ranger(score_obj, data, outcome)
  } else if (score_obj$engine == "partykit") {
    score_obj$score_type <- "imp_rf_conditional"
    imp <- get_imp_rf_partykit(score_obj, data, formula)
  } else if (score_obj$engine == "aorsf") {
    score_obj$score_type <- "imp_rf_oblique"
    imp <- get_imp_rf_aorsf(score_obj, data, formula)
  }
  res <- make_scores_forest_importance(
    score_obj$score_type,
    imp,
    outcome,
    predictors
  )
  res
}
