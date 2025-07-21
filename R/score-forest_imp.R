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
    trees = S7::new_property(S7::class_numeric, default = 10), # TODO May need to set other default
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

#' Scoring via random forest, conditional random forest, or oblique random forest
#'
#' @description
#'
#' These objects are used when either:
#'
#' - The predictors are numeric and the outcome is a factor/category, or
#' - The predictors are factors and the outcome is numeric.
#'
#' In either case, a random forest (via [ranger::ranger()], [partykit::cforest()],
#' or [aorsf::orsf()]) is created with the proper variable roles, and the feature
#' importance scores are computed. The larger values are associated with more
#' important predictors.
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

S7::method(fit, class_score_imp_rf) <- function(object, formula, data, ...) {
  analysis_data <- process_all_data(formula, data = data)

  # Note that model.frame() places the outcome(s) as the first column(s)
  predictors <- names(analysis_data)[-1]
  outcome <- names(analysis_data)[1]

  if (object@score_type == "imp_rf") {
    imp <- get_imp_rf_ranger(
      object,
      data = analysis_data,
      outcome = outcome
    )
  } else if (object@score_type == "imp_rf_conditional") {
    imp <- get_imp_rf_partykit(object, data = analysis_data, formula = formula)
  } else if (object@score_type == "imp_rf_oblique") {
    imp <- get_imp_rf_aorsf(object, data = analysis_data, formula = formula)
  }

  score <- imp[predictors]
  score[is.na(score)] <- 0
  score <- stats::setNames(score, predictors) # TODO Confirm this is the right approach

  res <- named_vec_to_tibble(score, object@score_type, outcome)

  object@results <- res
  object
}

get_imp_rf_ranger <- function(object, data, outcome) {
  if (object@score_type == "imp_rf") {
    importance_type = "permutation"
  } # TODO Allow option for importance = c("impurity")

  y <- data[[outcome]]
  X <- data[setdiff(names(data), outcome)]
  fit <- ranger::ranger(
    x = X,
    y = y,
    num.trees = object@trees,
    mtry = object@mtry,
    importance = importance_type,
    min.node.size = object@min_n,
    classification = object@mode == "classification",
    seed = object@seed
  )
  imp <- fit$variable.importance
  imp
}

get_imp_rf_partykit <- function(object, data, formula) {
  fit <- partykit::cforest(
    formula = formula,
    data = data,
    control = partykit::ctree_control(minsplit = object@min_n), # TODO Eventually have user pass in ctree_control()
    ntree = object@trees,
    mtry = object@mtry,
  )
  imp <- partykit::varimp(fit, conditional = TRUE)
  imp
}

get_imp_rf_aorsf <- function(object, data, formula) {
  if (object@score_type == "imp_rf_oblique") {
    importance_type = "permute"
  } # TODO Allow option for importance = c("none", "anova", "negate")

  fit <- aorsf::orsf(
    formula = formula,
    data = data,
    n_tree = object@trees,
    n_retry = object@mtry,
    importance = importance_type
  )
  imp <- fit$importance
  imp
}
