#' Title
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
score_aov <- function(
  range = c(0, Inf),
  trans = NULL,
  score_type = "fstat", # Move c() here later. Add validator. Document it.
  direction = "maximize"
) {
  new_score_obj(
    subclass = c("any"),
    outcome_type = c("numeric", "factor"),
    predictor_type = c("numeric", "factor"),
    case_weights = FALSE, # TODO
    range = range,
    inclusive = c(TRUE, TRUE),
    fallback_value = Inf,
    score_type = score_type, #c("fstat", "pval"),
    trans = NULL, # TODO
    sorts = NULL, # TODO
    direction = c("maximize", "minimize", "target"),
    deterministic = TRUE,
    tuning = FALSE,
    ties = NULL,
    calculating_fn = get_single_f_stat,
    label = c(score_aov = "ANOVA F-statistics and p-values")
  )
}

flip_if_needed_aov <- function(x, y) {
  if (is.factor(y) && is.numeric(x)) {
    list(predictor = y, outcome = x)
  } else {
    list(predictor = x, outcome = y)
  }
}

get_single_f_stat <- function(predictor, outcome) {
  flipped <- flip_if_needed_aov(x = predictor, y = outcome)
  outcome <- flipped$outcome
  predictor <- flipped$predictor

  fit <- stats::lm(outcome ~ predictor)
  res <- stats::anova(fit)$`F value`[1] # summary(fit)$fstatistic[1] |> as.numeric()
  return(res)
}

get_single_p_val <- function(predictor, outcome) {
  flipped <- flip_if_needed_aov(x = predictor, y = outcome)
  outcome <- flipped$outcome
  predictor <- flipped$predictor

  fit <- stats::lm(outcome ~ predictor)
  res <- stats::anova(fit)$`Pr(>F)`[1]
  return(res)
}

get_scores_aov <- function(score_obj, data, outcome) {
  if (score_obj$score_type == "fstat") {
    score_obj$calculating_fn <- get_single_f_stat
  } else if (score_obj$score_type == "pval") {
    score_obj$calculating_fn <- get_single_p_val
  }

  predictors <- setdiff(names(data), outcome)

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

      score_obj$calculating_fn(predictor_col, outcome_col) #get_single_f_stat(predictor_col, outcome_col)
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
