filter_cor <- function(
  range = c(-1, 1),
  trans = NULL,
  score_type = "pearson",
  direction = "maximize"
) {
  new_filters_score(
    subclass = c("any"),
    outcome_type = "numeric",
    predictor_type = "numeric",
    case_weights = FALSE, # To do
    range = range,
    inclusive = c(TRUE, TRUE),
    fallback_value = 1,
    score_type = score_type, # c("pearson", "spearman"),
    trans = NULL, # To do
    sorts = NULL, # To do
    direction = c("maximize", "minimize", "target"),
    deterministic = TRUE,
    tuning = FALSE,
    ties = NULL,
    calculating_fn = get_cor,
    label = c(filter_aov = "Filter method based on correlation scores")
  )
}

get_pearson <- function(predictor, outcome) {
  res <- cor(predictor, outcome, method = "pearson")
  return(res)
}

get_spearman <- function(predictor, outcome) {
  res <- cor(predictor, outcome, method = "spearman")
  return(res)
}

get_score_cor <- function(filter_obj, data, outcome) {
  if (filter_obj$score_type == "pearson") {
    filter_obj$calculating_fn <- get_pearson
  } else if (filter_obj$score_type == "spearman") {
    filter_obj$calculating_fn <- get_spearman
  }
  predictors <- setdiff(names(data), outcome)

  score <- purrr::map_dbl(
    purrr::set_names(predictors),
    ~ {
      predictor_col <- data[[.x]]
      outcome_col <- data[[outcome]]

      if (is.factor(outcome_col) || is.factor(predictor_col)) {
        return(NA_real_)
      }

      filter_obj$calculating_fn(predictor_col, outcome_col)
    }
  )
  names <- names(score)
  res <- dplyr::tibble(
    name = filter_obj$score_type,
    score = unname(score),
    outcome = outcome,
    predictor = names
  )
}

# To do: Confirm the structure, i.e., score_type =
# To do: Add methods
# To do: Add test
