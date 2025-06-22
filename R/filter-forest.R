score_forest <- function(
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
    score_type = score_type, # c("pearson", "spearman"),
    trans = NULL, # TODO
    sorts = NULL, # TODO
    direction = c("maximize", "minimize", "target"),
    deterministic = TRUE,
    tuning = TRUE,
    ties = NULL,
    calculating_fn = get_score_importance,
    label = c(score_aov = "Random Forest importance scores")
  )
}

get_score_importance <- function(score_obj, data, outcome, engine = "ranger") {
  outcome_name <- outcome |> as.name()
  formula <- as.formula(paste(outcome_name, "~ ."))

  if (score_obj$engine == "ranger") {
    fit <- ranger::ranger(
      formula = formula,
      data = data,
      num.trees = 10, # TODO Might want to standardize these first
      mtry = 2,
      importance = "permutation", # TODO = score_obj$score_type
      min.node.size = NULL,
      classification = TRUE # TODO Need to be =c(TRUE, FALSE)
    )
    res <- fit$variable.importance
  } else if (score_obj$engine == "partykit") {
    fit <- partykit::cforest(
      formula = formula,
      data = data,
      control = ctree_control(
        minsplit = 1L,
        minbucket = 1L,
      ),
      ntree = 10,
      mtry = 2
    )
    res <- varimp(fit, conditional = TRUE) # conditional permutation
    res # TODO Not always return scores for all predictors. Probably need to do something.
  }

  # TODO Add aorsf::orsf

  score <- unname(res)
  names <- names(res)
  res <- dplyr::tibble(
    name = rep(score_obj$score_type, length(score)),
    score = score,
    outcome = rep(outcome, length(score)),
    predictor = names
  )
}

# TODO Add engine = c("ranger", "partykit", "aorsf")
# TODO Expand score_obj$score_type
