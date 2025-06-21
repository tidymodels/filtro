score_cor <- function(
  range = c(-1, 1),
  trans = NULL,
  score_type = "pearson",
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
    calculating_fn = get_cor,
    label = c(score_aov = "Correlation scores")
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

get_score_cor <- function(score_obj, data, outcome) {
  if (score_obj$score_type == "pearson") {
    score_obj$calculating_fn <- get_pearson
  } else if (score_obj$score_type == "spearman") {
    score_obj$calculating_fn <- get_spearman
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

      score_obj$calculating_fn(predictor_col, outcome_col)
    }
  )
  names <- names(score)
  res <- dplyr::tibble(
    name = score_obj$score_type,
    score = unname(score),
    outcome = outcome,
    predictor = names
  )
}

# TODO Confirm the structure, i.e., score_type =
# TODO Add methods
# TODO Add test
