# Create a subclass and add new properties (i.e., neg_log10) specific for aov
new_score_obj_aov_v2 <- S7::new_class(
  "new_score_obj_aov_v2",
  parent = new_score_obj_v2,
  properties = list(
    neg_log10 = S7::new_property(S7::class_logical, default = FALSE)
  )
)
# User call
class(new_score_obj_aov_v2())
new_score_obj_aov_v2()
b <- new_score_obj_aov_v2()
b@score_type

# helper function score_aov_v2(). Will replace score_aov().
score_aov_v2 <- function(
  range = c(0, Inf),
  fallback_value = Inf,
  score_type = "fstat",
  direction = "maximize",
  neg_log10 = FALSE
) {
  new_score_obj_aov_v2(
    outcome_type = c("numeric", "factor"),
    predictor_type = c("numeric", "factor"),
    case_weights = FALSE,
    range = range,
    inclusive = c(TRUE, TRUE),
    fallback_value = fallback_value,
    score_type = score_type,
    direction = direction,
    #trans = # Cannot set NULL. Otherwise S7 complains
    #sorts =
    deterministic = TRUE,
    tuning = FALSE,
    #ties =
    calculating_fn = function(x) {}, # Otherwise S7 complains
    neg_log10 = neg_log10,
    label = c(score_aov = "ANOVA F-test F-statistics and p-values")
  )
}
# User call
class(score_aov_v2())
score_aov_v2()
score_obj <- score_aov_v2(score_type = "fstat")
score_obj@score_type # Developer call
score_obj <- score_aov_v2(score_type = "pval")
score_obj@score_type # Developer call
score_obj <- score_aov_v2(score_type = "fstat")
score_obj@score_type # Developer call

# # helper function get_scores_aov() and run into error: Use @ instead of #
# score_res <- get_scores_aov(
#   score_obj,
#   data = ames_subset,
#   outcome = "Sale_Price"
# )

# helper function get_scores_aov_v2(). Will replace get_scores_aov().
# bascially everything $ has to be replaced with @.
get_scores_aov_v2 <- function(score_obj, data, outcome, ...) {
  if (score_obj@score_type == "fstat") {
    score_obj@calculating_fn <- get_single_f_stat
  } else if (score_obj@score_type == "pval") {
    score_obj@calculating_fn <- get_single_p_val
  }

  predictors <- setdiff(names(data), outcome)

  score <- purrr::map_dbl(
    purrr::set_names(predictors),
    \(x) map_score_aov(data, x, outcome, score_obj@calculating_fn)
  )

  # Do we need score <- stats::p.adjust(score) here too?

  if (
    score_obj@score_type == "pval" &&
      (is.null(score_obj@neg_log10) || isTRUE(score_obj@neg_log10))
  ) {
    score <- -log10(score)
  }

  res <- make_scores_aov(score_obj@score_type, score, outcome, predictors)
  res
}

# User call
score_obj <- score_aov_v2(score_type = "fstat")
score_res <- get_scores_aov_v2(
  score_obj,
  data = ames_subset,
  outcome = "Sale_Price"
)
score_res

score_obj <- score_aov_v2(score_type = "pval")
score_res <- get_scores_aov_v2(
  score_obj,
  data = ames_subset,
  outcome = "Sale_Price"
)
score_res

score_obj <- score_aov_v2(score_type = "pval", neg_log10 = TRUE)
score_res <- get_scores_aov_v2(
  score_obj,
  data = ames_subset,
  outcome = "Sale_Price"
)
score_res
