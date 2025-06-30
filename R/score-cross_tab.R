score_cross_tab <- function(
  range = c(0, 1),
  trans = NULL,
  score_type = "chisq",
  direction = "minimize"
) {
  new_score_obj(
    subclass = c("cat_cat"),
    outcome_type = "factor",
    predictor_type = "factor",
    case_weights = FALSE, # TODO
    range = range,
    inclusive = c(TRUE, TRUE),
    fallback_value = 1,
    score_type = score_type, # c("chisq", "fisher"),
    trans = NULL, # TODO
    sorts = NULL, # TODO
    direction = c("maximize", "minimize", "target"),
    deterministic = TRUE,
    tuning = FALSE,
    ties = NULL,
    calculating_fn = get_single_chisq,
    label = c(score_crosstab = "Cross tabulation p-values")
  )
}

get_single_chisq <- function(predictor, outcome) {
  tab <- table(predictor, outcome) # TODO sample(predictor); Check Slack 06-23
  res <- suppressWarnings(stats::chisq.test(tab)$p.value)
  return(res)
}

get_single_fisher <- function(predictor, outcome) {
  tab <- table(predictor, outcome) # TODO sample(predictor); Check Slack 06-23
  res <- suppressWarnings(stats::fisher.test(tab)$p.value)
  return(res)
}

map_score_cross_tab <- function(data, predictor, outcome, calculating_fn) {
  predictor_col <- data[[predictor]]
  outcome_col <- data[[outcome]]

  if (is.numeric(outcome_col) || is.numeric(predictor_col)) {
    return(NA_real_)
  }
  # if (
  #   length(levels(outcome_col)) > 2 || length(levels(predictor_col)) > 2
  # ) {
  #   return(NA_real_)
  # }

  res <- calculating_fn(predictor_col, outcome_col)
  res
}

make_scores_cross_tab <- function(score_type, score, outcome, predictors) {
  res <- dplyr::tibble(
    name = score_type,
    score = unname(score),
    outcome = outcome,
    predictor = predictors
  )
  res
}

get_scores_cross_tab <- function(
  score_obj,
  data,
  outcome,
  ... # i.e., score_obj$fdr
) {
  if (score_obj$score_type == "chisq") {
    # TODO Should I move this elsewhere?
    score_obj$calculating_fn <- get_single_chisq
  } else if (score_obj$score_type == "fisher") {
    score_obj$calculating_fn <- get_single_fisher
  }
  predictors <- setdiff(names(data), outcome)

  score <- purrr::map_dbl(
    purrr::set_names(predictors),
    \(x) map_score_cross_tab(data, x, outcome, score_obj$calculating_fn)
  )

  if (score_obj$fdr == TRUE) {
    # TODO Should I move this elsewhere?
    score <- stats::p.adjust(score)
  }

  res <- make_scores_cross_tab(score_obj$score_type, score, outcome, predictors)
  res
}
