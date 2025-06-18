filter_roc_auc <- function(range = c(0, 1), trans = NULL) {
  new_filters_score(
    subclass = c("any"),
    outcome_type = c("numeric", "factor"),
    predictor_type = c("numeric", "factor"),
    case_weights = FALSE,
    range = range,
    inclusive = c(TRUE, TRUE),
    fallback_value = 1,
    score_type = "auc",
    trans = NULL, # To do
    sorts = NULL, # To do
    direction = "maximize",
    deterministic = TRUE,
    tuning = FALSE,
    ties = NULL,
    calculating_fn = get_roc_auc,
    label = c(filter_aov = "Filter method based on ROC AUC scores")
  )
}

flip_if_needed <- function(x, y) {
  is_factor <- function(v) is.factor(v) && length(levels(v)) >= 2

  if (is_factor(y) && is.numeric(x)) {
    list(predictor = x, outcome = y)
  } else {
    list(predictor = y, outcome = x)
  }
}

get_roc_auc <- function(predictor, outcome) {
  flipped <- flip_if_needed(x = predictor, y = outcome)
  outcome <- flipped$outcome
  predictor <- flipped$predictor

  if (length(levels(outcome)) == 2) {
    roc <- pROC::roc(outcome, predictor, direction = "auto", quiet = TRUE)
    res <- pROC::auc(roc) |> as.numeric()
  } else {
    roc <- pROC::multiclass.roc(
      outcome,
      predictor,
      direction = "auto",
      quiet = TRUE
    )
    res <- pROC::auc(roc) |> as.numeric()
  }
  res
}

get_score <- function(filter_obj, data, outcome) {
  predictors <- setdiff(names(data), outcome)

  # score <- purrr::map_dbl(
  #   predictors,
  #   ~ get_roc_auc(data[[.x]], data[[outcome]])
  # )

  score <- purrr::map_dbl(
    purrr::set_names(predictors),
    ~ {
      predictor_col <- data[[.x]]
      outcome_col <- data[[outcome]]

      if (is.factor(outcome_col) && !is.numeric(predictor_col)) {
        return(NA_real_)
      }

      if (is.numeric(outcome_col) && !is.factor(predictor_col)) {
        return(NA_real_)
      }

      get_roc_auc(predictor_col, outcome_col)
    }
  )

  res <- dplyr::tibble(
    name = filter_obj$score_type,
    score = score,
    outcome = outcome,
    predictor = names(score)
  )
}

calc_score <- function(filter_obj, data, outcome) {
  res <- get_score(filter_obj, data, outcome)
  filter_obj$res <- res
  filter_obj
}

#' @noRd
#' @export
print.score <- function(x, ...) {
  # Option 2
  # Add ifelse to tell if it has been trained or not

  print_score_label(x) #cat(x$label)
  invisible(x)
}

print_score_label <- function(x) {
  cli::cli_text("{x$label}")
}

# May want fit_score. (or calc_score.)
# calc_score <- function(x, ...) {
#   UseMethod("calc_score")
# }
