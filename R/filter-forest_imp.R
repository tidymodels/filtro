score_forest_imp <- function(
  range = c(0, Inf),
  trans = NULL,
  score_type = "permutation", # c("permutation", "impurity", ...)
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
    score_type = score_type,
    trans = NULL, # TODO
    sorts = NULL, # TODO
    direction = c("maximize", "minimize", "target"),
    deterministic = FALSE,
    tuning = TRUE,
    ties = NULL,
    calculating_fn = NULL,
    label = c(score_aov = "Random Forest importance scores")
  )
}

# if (score_obj$engine == "ranger") {
#   options <- list(
#     mtry = score_obj$mtry,
#     num.trees = score_obj$trees,
#     min.node.size = score_obj$min_n,
#     classification = TRUE
#   )
# } else if (score_obj$engine == "partykit") {
#   options <- list(
#     mtry = score_obj$mtry,
#     ntree = score_obj$trees,
#     control = partykit::ctree_control(minsplit = score_obj$min_n)
#   )
# } else if (score_obj$engine == "aorsf") {
#   options <- list(
#     mtry = score_obj$mtry,
#     n_tree = score_obj$trees,
#     min_events = score_obj$min_n,
#     importance = "permute"
#   )
# }
# options

get_score_forest_importance <- function(
  score_obj,
  data,
  outcome,
  ... # i.e., score_obj$engine, score_obj$trees, score_obj$mtry, score_obj$min_n
) {
  outcome_name <- outcome |> as.name()
  formula <- as.formula(paste(outcome_name, "~ .")) # TODO Avoid formula method if possible because of slowness caused by design matrix

  if (score_obj$engine == "ranger") {
    fit <- ranger::ranger(
      # TODO Pass in expression
      formula = formula,
      data = data,
      num.trees = score_obj$trees,
      mtry = score_obj$mtry,
      importance = score_obj$score_type, # TODO importance = c(impurity)
      min.node.size = score_obj$min_n,
      classification = TRUE, # TODO classification = FALSE
      seed = 42 # TODO Add this to pass tests. Remove later.
    )
    res <- fit$variable.importance
  } else if (score_obj$engine == "partykit") {
    fit <- partykit::cforest(
      formula = formula,
      data = data,
      control = ctree_control(minsplit = score_obj$min_n), # TODO Eventually have user pass in ctree_control()
      ntree = score_obj$trees,
      mtry = score_obj$mtry,
    )
    res <- partykit::varimp(fit, conditional = TRUE) # TODO conditional = FALSE
    # TODO Not always return score for predictor. Replace with 0, instead of NA.
  } else if (score_obj$engine == "aorsf") {
    fit <- aorsf::orsf(
      formula = formula,
      data = data,
      n_tree = score_obj$trees,
      n_retry = score_obj$mtry,
      importance = "permute" # TODO = c("none", "anova", "negate")
    )
    res <- fit$importance # orsf_vi_permute(fit)
  }
  score <- unname(res)
  names <- names(res)
  res <- dplyr::tibble(
    name = rep(score_obj$score_type, length(score)),
    score = score,
    outcome = rep(outcome, length(score)),
    predictor = names
  )
}
