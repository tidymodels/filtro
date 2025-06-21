score_cross_tab <- function(
  # TODO Change to score_*
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
    calculating_fn = get_cor,
    label = c(score_aov = "Cross tabulation p-values")
  )
}

get_chisq <- function(predictor, outcome) {
  tab <- table(predictor, outcome)
  res <- stats::chisq.test(tab)$p.value
  return(res)
}

get_fisher <- function(predictor, outcome) {
  tab <- table(predictor, outcome)
  res <- stats::fisher.test(tab)$p.value
  return(res)
}

get_score_cross_tab <- function(score_obj, data, outcome) {
  if (score_obj$score_type == "chisq") {
    score_obj$calculating_fn <- get_chisq
  } else if (score_obj$score_type == "fisher") {
    score_obj$calculating_fn <- get_fisher
  }
  predictors <- setdiff(names(data), outcome)

  score <- purrr::map_dbl(
    purrr::set_names(predictors),
    ~ {
      predictor_col <- data[[.x]]
      outcome_col <- data[[outcome]]

      if (is.numeric(outcome_col) || is.numeric(predictor_col)) {
        return(NA_real_)
      }
      if (
        length(levels(outcome_col)) > 2 || length(levels(predictor_col)) > 2
      ) {
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
