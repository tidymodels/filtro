#' Title
#'
#' @param range NULL
#' @param trans NULL
#'
#' @returns NULL
#' @export
#'
#' @examples NULL
score_roc_auc <- function(range = c(0, 1), trans = NULL) {
  new_score_obj(
    subclass = c("any"),
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
    label = c(score_aov = "ROC AUC scores")
  )
}

flip_if_needed <- function(predictor, outcome) {
  # TODO Move to utilities.R
  if (is.factor(outcome) && is.numeric(predictor)) {
    list(predictor = predictor, outcome = outcome)
  } else {
    list(predictor = outcome, outcome = predictor)
  }
}

get_single_roc_auc <- function(predictor, outcome, ...) {
  flipped <- flip_if_needed(predictor, outcome)
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
    #~ map_score_roc_auc(data, .x, outcome)
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

# #' @noRd
# #' @export
# fit.score_obj <- function(score_obj, data, outcome, ...) {
#   UseMethod("fit_score")
# }

# fit_score <- function(score_obj, data, outcome, ...) {
#   res <- get_scores_roc_auc(score_obj, data, outcome)
#   score_obj$res <- res
#   score_obj
# }

# fit_score_res <- function(score_obj) {
#   score_obj$res
# }

# fit_score_scaling <- function(score_obj) {
#   if (is.null(score_obj$trans)) {
#     trans <- scales::transform_identity()
#   } else {
#     trans <- score_obj$trans
#   }
#   score_obj$res |>
#     mutate(score = trans$transform(score))
# }

# fit_score_arranging <- function(score_obj, target = 0.993) {
#   if (score_obj$direction == "maximize") {
#     score_obj$res |> arrange(desc(score))
#   } else if (score_obj$direction == "minimize") {
#     score_obj$res |> arrange(score)
#   } else if (score_obj$direction == "target") {
#     score_obj$res |> arrange(abs(score - target))
#   }
# }

# fit_score_filtering <- function(score_obj, num_terms = 2, target = 0.993) {
#   if (score_obj$direction == "maximize") {
#     score_obj$res |> arrange(desc(score)) |> slice_head(n = num_terms)
#   } else if (score_obj$direction == "minimize") {
#     score_obj$res |> arrange(score) |> slice_head(n = num_terms)
#   } else if (score_obj$direction == "target") {
#     score_obj$res |> arrange(abs(score - target)) |> slice_head(n = num_terms)
#   }
# }

# fit_score_filtering_v2 <- function(score_obj, num_terms = 2, target = 0.993) {
#   if (score_obj$direction == "maximize") {
#     score_obj$res |> slice_max(score, n = num_terms)
#   } else if (score_obj$direction == "minimize") {
#     score_obj$res |> slice_min(score, n = num_terms)
#   } else if (score_obj$direction == "target") {
#     score_obj$res |> arrange(abs(score - target)) |> slice_head(n = num_terms)
#   }
# }

# # TODO Use an S3 generic
# # Right now the fit_score_* are independent of one another.
