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

# To do: Helper function to check if a vector is a factor with 2+ levels
# flip x, y if needed, e.g., pROC::roc(x, y) and pROC::roc(y, x). Pass to get_roc_auc.
# is_factor <- function(v) is.factor(v) && length(levels(v)) >= 2

# if (is_factor(y) && is.numeric(x)) {
#   outcome <- y
#   predictor <- x
# } else if (is.numeric(y) && is_factor(x)) {
#   outcome <- x
#   predictor <- y
# } else {
#   stop(
#     "One argument must be a factor with 2+ levels and the other numeric."
#   )
# }

get_roc_auc <- function(predictor, outcome) {
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

      if (!is.numeric(predictor_col)) {
        return(NA_real_)
      }

      get_roc_auc(predictor_col, data[[outcome]])
    }
  )

  # score <- vapply(
  #   predictors,
  #   function(predictor) {
  #     predictor_col <- data[[predictor]]

  #     if (!is.numeric(predictor_col)) {
  #       return(NA_real_)
  #     }

  #     filter_obj$calculating_fn(predictor_col, data[[outcome]])
  #   },
  #   FUN.VALUE = numeric(1)
  # )

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
