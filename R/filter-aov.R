filter_aov <- function(
  range = c(0, Inf),
  trans = NULL,
  direction = "maximize"
) {
  new_cat_num_score(
    outcome_type = c("numeric", "factor"),
    predictor_type = c("numeric", "factor"),
    case_weights = case_weights,
    range = range,
    inclusive = c(TRUE, TRUE),
    fallback_value = Inf,
    score_type = score_type,
    trans = trans,
    sorts = sorts,
    direction = direction,
    deterministic = TRUE,
    tuning = FALSE,
    ties = ties,
    label = c(filter_aov = "Filter method based on ANOVA F-statistics")
  )
}

get_f_stat <- function(x, y) {
  # Helper function to check if a vector is a factor with 2+ levels
  is_factor <- function(v) is.factor(v) && length(levels(v)) >= 2

  if (is_factor(y) && is.numeric(x)) {
    outcome <- x
    predictor <- y
  } else if (is.numeric(y) && is_factor(x)) {
    outcome <- y
    predictor <- x
  } else {
    stop(
      "One argument must be a factor with 2+ levels and the other numeric."
    )
  }

  if (length(levels(outcome)) == 2) {
    fit <- lm(outcome ~ predictor)
    res <- summary(fit)$fstatistic[1]
  } else {
    fit <- lm(outcome ~ predictor)
    res <- summary(fit)$fstatistic[1]
  }
  res
}

# To do:
# Need a for loop or map here to handle all predictor-outcome pairs

# To do:
# get_p_val <- function() {
#   ...
# }
