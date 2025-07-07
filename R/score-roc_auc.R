#' Compute area under the Receiver Operating Characteristic curve (ROC AUC)
#'
#' @param range NULL
#' @param trans NULL
#'
#' @returns NULL
#' @export
#'
#' @examples NULL
score_roc_auc <- function(
  range = c(0, 1),
  trans = NULL
) {
  new_score_obj(
    subclass = c("any"), # TODO
    outcome_type = c("numeric", "factor"),
    predictor_type = c("numeric", "factor"),
    case_weights = FALSE, # TODO
    range = range,
    inclusive = c(TRUE, TRUE),
    fallback_value = 1,
    score_type = "roc_auc",
    trans = NULL, # TODO
    sorts = NULL, # TODO
    direction = "maximize",
    deterministic = TRUE,
    tuning = FALSE,
    ties = NULL,
    calculating_fn = get_single_roc_auc,
    label = c(score_rocauc = "ROC AUC scores")
  )
}

flip_if_needed_roc_auc <- function(predictor, outcome) {
  # TODO Move to utilities.R
  if (is.factor(outcome) && is.numeric(predictor)) {
    list(predictor = predictor, outcome = outcome)
  } else {
    list(predictor = outcome, outcome = predictor)
  }
}

get_single_roc_auc <- function(predictor, outcome, ...) {
  flipped <- flip_if_needed_roc_auc(predictor, outcome)
  outcome <- flipped$outcome
  predictor <- flipped$predictor

  if (length(levels(outcome)) == 2) {
    # TODO if else will change once we pass case_weights = in later
    roc <- pROC::roc(outcome, predictor, direction = "auto", quiet = TRUE, ...)
  } else {
    roc <- pROC::multiclass.roc(
      outcome,
      predictor,
      direction = "auto",
      quiet = TRUE,
      ...
    )
  }
  res <- pROC::auc(roc) |> as.numeric()
  res
}

map_score_roc_auc <- function(data, predictor, outcome) {
  predictor_col <- data[[predictor]]
  outcome_col <- data[[outcome]]

  if (is.factor(outcome_col) && !is.numeric(predictor_col)) {
    return(NA_real_)
  }

  if (is.numeric(outcome_col) && !is.factor(predictor_col)) {
    return(NA_real_)
  }

  res <- get_single_roc_auc(predictor_col, outcome_col)
  res
}

make_scores_roc_auc <- function(score_type, score, outcome, predictors) {
  res <- dplyr::tibble(
    name = score_type,
    score = unname(score),
    outcome = outcome,
    predictor = predictors
  )
  res
}

get_scores_roc_auc <- function(score_obj, data, outcome) {
  predictors <- setdiff(names(data), outcome)

  score <- purrr::map_dbl(
    purrr::set_names(predictors),
    \(x) map_score_roc_auc(data, x, outcome)
  )

  res <- make_scores_roc_auc(score_obj$score_type, score, outcome, predictors)
  res
}

#' @noRd
#' @export
print.score_obj <- function(x, ...) {
  # Option 2
  # Add ifelse to tell if it has been trained or not

  print_score_label(x) #cat(x$label)
  invisible(x)
}

print_score_label <- function(x) {
  cli::cli_text("{x$label}")
}
