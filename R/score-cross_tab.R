#' Create a score object for cross tabulation p-values
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
score_cross_tab <- function(
  range = c(0, 1),
  trans = NULL,
  score_type = "pval_chisq", # Move c() here later. Add validator. Document it.
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
    score_type = score_type, # c("pval_chisq", "pval_fisher"),
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

#' Compute cross tabulation p-value scores using Fisher’s exact test and the chi-squared test
#'
#' @param score_obj NULL
#'
#' @param data NULL
#' @param outcome NULL
#' @param ... NULL
#'
#' @export
get_scores_cross_tab <- function(
  score_obj,
  data,
  outcome,
  ... # i.e., score_obj$fdr
) {
  if (score_obj$score_type == "pval_chisq") {
    score_obj$calculating_fn <- get_single_chisq
  } else if (score_obj$score_type == "pval_fisher") {
    score_obj$calculating_fn <- get_single_fisher
  }
  predictors <- setdiff(names(data), outcome)

  score <- purrr::map_dbl(
    purrr::set_names(predictors),
    \(x) map_score_cross_tab(data, x, outcome, score_obj$calculating_fn)
  )

  if (score_obj$fdr == TRUE) {
    score <- stats::p.adjust(score)
  }

  if (is.null(score_obj$neg_log_pval) || isTRUE(score_obj$neg_log_pval)) {
    score <- -log10(score)
  }

  res <- make_scores_cross_tab(score_obj$score_type, score, outcome, predictors)
  res
}
