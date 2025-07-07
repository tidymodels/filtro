#' Compute Pearson or Spearman correlation coefficients
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
score_cor <- function(
  range = c(-1, 1),
  trans = NULL,
  score_type = "pearson", # Move c() here later. Add validator. Document it.
  direction = "maximize"
) {
  new_score_obj(
    subclass = c("num_num"),
    outcome_type = "numeric",
    predictor_type = "numeric",
    case_weights = FALSE, # TODO
    range = range,
    inclusive = c(TRUE, TRUE),
    fallback_value = 1,
    score_type = score_type, # c("pearson", "spearman"),
    trans = NULL, # TODO
    sorts = NULL, # TODO
    direction = c("maximize", "minimize", "target"),
    deterministic = TRUE,
    tuning = FALSE,
    ties = NULL,
    calculating_fn = get_single_pearson,
    label = c(score_cor = "Correlation scores")
  )
}

get_single_pearson <- function(predictor, outcome) {
  res <- stats::cor(predictor, outcome, method = "pearson")
  return(res)
}

get_single_spearman <- function(predictor, outcome) {
  res <- stats::cor(predictor, outcome, method = "spearman")
  return(res)
}

map_score_cor <- function(data, predictor, outcome, calculating_fn) {
  predictor_col <- data[[predictor]]
  outcome_col <- data[[outcome]]

  if (is.factor(outcome_col) || is.factor(predictor_col)) {
    return(NA_real_)
  }

  res <- calculating_fn(predictor_col, outcome_col)
  res
}

make_scores_cor <- function(score_type, score, outcome, predictors) {
  res <- dplyr::tibble(
    name = score_type,
    score = unname(score),
    outcome = outcome,
    predictor = predictors
  )
  res
}

#' @export
get_scores_cor <- function(score_obj, data, outcome) {
  if (score_obj$score_type == "pearson") {
    # TODO Should I move this elsewhere?
    score_obj$calculating_fn <- get_single_pearson
  } else if (score_obj$score_type == "spearman") {
    score_obj$calculating_fn <- get_single_spearman
  }
  predictors <- setdiff(names(data), outcome)

  score <- purrr::map_dbl(
    purrr::set_names(predictors),
    \(x) map_score_cor(data, x, outcome, score_obj$calculating_fn)
  )

  res <- make_scores_cor(score_obj$score_type, score, outcome, predictors)
  res
}
