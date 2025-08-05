skip()

ames_subset <- helper_ames()
ames_subset <- ames_subset |>
  dplyr::mutate(Sale_Price = log10(Sale_Price))

# ANOVA p-value
ames_aov_pval_res <-
  score_aov_pval |>
  fit(Sale_Price ~ ., data = ames_subset)
ames_aov_pval_res@results

# Pearson correlation
ames_cor_pearson_res <-
  score_cor_pearson |>
  fit(Sale_Price ~ ., data = ames_subset)
ames_cor_pearson_res@results

# Forest importance
set.seed(42)
ames_imp_rf_reg_res <-
  score_imp_rf |>
  fit(Sale_Price ~ ., data = ames_subset)
ames_imp_rf_reg_res@results

# Information gain
score_info_gain_reg <- score_info_gain
score_info_gain_reg@mode <- "regression"

ames_info_gain_reg_res <-
  score_info_gain_reg |>
  fit(Sale_Price ~ ., data = ames_subset)
ames_info_gain_reg_res@results

# Create a list
class_score_list <- list(
  ames_aov_pval_res,
  ames_cor_pearson_res,
  ames_imp_rf_reg_res,
  ames_info_gain_reg_res
)

# Fill safe values
ames_scores_results <- class_score_list |>
  filtro::fill_safe_values() |>
  # TODO Write a helper at current line to transform scores if needed,
  # e.g., abs(cor_*), max(roc_auc, 1 - roc_auc)
  # TODO Remove the next line after removing outcome from score-*.R
  dplyr::select(-outcome)
ames_scores_results

# show_best_desirability_prop
# TODO Default prop_terms = 1. Can change later.

library(desirability2)

show_best_desirability_prop(
  ames_scores_results,
  maximize(cor_pearson, low = 0, high = 1)
)

show_best_desirability_prop(
  ames_scores_results,
  maximize(cor_pearson, low = 0, high = 1),
  maximize(imp_rf)
)

show_best_desirability_prop(
  ames_scores_results,
  maximize(cor_pearson, low = 0, high = 1),
  maximize(imp_rf),
  maximize(infogain)
)

show_best_desirability_prop(
  ames_scores_results,
  maximize(cor_pearson, low = 0, high = 1),
  maximize(imp_rf),
  maximize(infogain),
  prop_terms = 0.2
)

show_best_desirability_prop(
  ames_scores_results,
  target(cor_pearson, low = 0.2, target = 0.255, high = 0.9)
)

show_best_desirability_prop(
  ames_scores_results,
  constrain(cor_pearson, low = 0.2, high = 1)
)

# show_best_desirability_num
# TODO Default num_terms = 5. Can change later.

show_best_desirability_num(
  ames_scores_results,
  maximize(cor_pearson, low = 0, high = 1)
)

show_best_desirability_num(
  ames_scores_results,
  maximize(cor_pearson, low = 0, high = 1),
  maximize(imp_rf)
)

show_best_desirability_num(
  ames_scores_results,
  maximize(cor_pearson, low = 0, high = 1),
  maximize(imp_rf),
  maximize(infogain)
)

show_best_desirability_num(
  ames_scores_results,
  maximize(cor_pearson, low = 0, high = 1),
  maximize(imp_rf),
  maximize(infogain),
  num_terms = 2
)
