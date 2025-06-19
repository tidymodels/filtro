filter_aov <- function(
  range = c(0, Inf),
  trans = NULL,
  direction = "maximize"
) {
  new_filters_score(
    subclass = c("any"),
    outcome_type = c("numeric", "factor"),
    predictor_type = c("numeric", "factor"),
    case_weights = FALSE, # To do
    range = range,
    inclusive = c(TRUE, TRUE),
    fallback_value = Inf,
    score_type = c("fstat"), # Add "pval"
    trans = NULL, # To do
    sorts = NULL, # To do
    direction = "maximize",
    deterministic = TRUE,
    tuning = FALSE,
    ties = NULL,
    calculating_fn = get_f_stat,
    label = c(filter_aov = "Filter method based on ANOVA F-statistics")
  )
}

flip_if_needed_aov <- function(x, y) {
  if (is.factor(y) && is.numeric(x)) {
    list(predictor = y, outcome = x)
  } else {
    list(predictor = x, outcome = y)
  }
}

get_f_stat <- function(predictor, outcome) {
  flipped <- flip_if_needed_aov(x = predictor, y = outcome)
  outcome <- flipped$outcome
  predictor <- flipped$predictor

  if (length(levels(predictor)) == 2) {
    fit <- lm(outcome ~ predictor)
    res <- anova(fit)$`F value`[1] # summary(fit)$fstatistic[1] |> as.numeric()
  } else {
    fit <- lm(outcome ~ predictor)
    res <- anova(fit)$`F value`[1] # summary(fit)$fstatistic[1] |> as.numeric()
  }
  res
}

get_score_aov <- function(filter_obj, data, outcome) {
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

      get_f_stat(predictor_col, outcome_col)
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

# To do:
# get_p_val <- function() {
#   ...
# }

get_p_val <- function(predictor, outcome) {
  flipped <- flip_if_needed_aov(x = predictor, y = outcome)
  outcome <- flipped$outcome
  predictor <- flipped$predictor

  if (length(levels(predictor)) == 2) {
    fit <- lm(outcome ~ predictor)
    res <- anova(fit)$`Pr(>F)`[1]
  } else {
    fit <- lm(outcome ~ predictor)
    res <- anova(fit)$`Pr(>F)`[1]
  }
  res
}

get_score_aov_p_val <- function(filter_obj, data, outcome) {
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

      get_p_val(predictor_col, outcome_col)
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
