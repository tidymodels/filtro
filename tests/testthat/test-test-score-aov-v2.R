ames_subset <- helper_ames()
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

score_obj <- score_aov_v2(
  score_type = "pval",
  neg_log10 = TRUE,
  direction = "minimize",
  fallback_value = 0
)
score_res <- get_scores_aov_v2(
  score_obj,
  data = ames_subset,
  outcome = "Sale_Price"
)
score_res
