filter_aov <- function(
  range = c(0, Inf),
  trans = NULL,
  direction = "maximize"
) {
  new_filters_score(
    subclass = c("any"),
    outcome_type = c("numeric", "factor"),
    predictor_type = c("numeric", "factor"),
    case_weights = case_weights,
    range = range,
    inclusive = c(TRUE, TRUE),
    fallback_value = Inf,
    score_type = score_type,
    trans = NULL, # To do
    sorts = NULL, # To do
    direction = direction,
    deterministic = TRUE,
    tuning = FALSE,
    ties = ties,
    calculating_fn = get_f_stat,
    label = c(filter_aov = "Filter method based on ANOVA F-statistics")
  )
}

# To do: Helper function to check if a vector is a factor with 2+ levels
# flip x, y if needed, e.g., lm(y~x) and lm(x~y). Pass to get_f_stat.

get_f_stat <- function(predictor, outcome) {
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
# Need a for loop or map to handle all predictor-outcome pairs

# To do:
# get_p_val <- function() {
#   ...
# }
