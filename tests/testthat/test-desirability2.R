# TODO Write these informal tests as formal tests after S7 refactor
ames_subset <- helper_ames()
ames_subset <- ames_subset |>
  dplyr::mutate(Sale_Price = log10(Sale_Price))

score_obj_aov <- filtro::score_aov(score_type = "pval")
score_res_aov <- filtro::get_scores_aov(
  score_obj_aov,
  data = ames_subset,
  outcome = "Sale_Price"
)
score_obj_aov <- score_obj_aov |> attach_score(score_res_aov)

score_obj_cor <- filtro::score_cor()
score_res_cor <- filtro::get_scores_cor(
  score_obj_cor,
  data = ames_subset,
  outcome = "Sale_Price"
)
score_obj_cor <- score_obj_cor |> filtro::attach_score(score_res_cor)

score_obj_imp <- filtro::score_forest_imp(mode = "regression")
score_res_imp <- get_scores_forest_importance(
  score_obj_imp,
  data = ames_subset,
  outcome = "Sale_Price"
)
score_obj_imp <- score_obj_imp |> filtro::attach_score(score_res_imp)

score_obj_info <- score_info_gain(mode = "regression")
score_res_info <- get_scores_info_gain(
  score_obj_info,
  data = ames_subset,
  outcome = "Sale_Price"
)
score_obj_info <- score_obj_info |> filtro::attach_score(score_res_info)

score_obj_list <- list(
  score_obj_aov,
  score_obj_cor,
  score_obj_imp,
  score_obj_info
)

scores_combined <- score_obj_list |> filtro::fill_safe_values()
scores_combined <- scores_combined |> dplyr::select(-outcome) # TODO Remove this after removing outcome from score-*.R

# Default prop_terms = 0.99 in order to compare item_selected()
prop_selected(scores_combined, maximize(pval))

prop_selected(
  scores_combined,
  maximize(pval),
  maximize(pearson, low = 0, high = 1)
)

prop_selected(
  scores_combined,
  maximize(pval),
  maximize(pearson, low = 0, high = 1),
  maximize(imp_rf)
)

# Would suggest keeping num_selected() just for reference. Plus, prop_selected() and num_selected()
# only differ in the argument n = vs, prop = in dplyr::slice_max()
num_selected(scores_combined, maximize(pval))

num_selected(
  scores_combined,
  maximize(pval),
  maximize(pearson, low = 0, high = 1)
)

num_selected(
  scores_combined,
  maximize(pval),
  maximize(pearson, low = 0, high = 1),
  maximize(imp_rf)
)

num_selected(
  scores_combined,
  maximize(pval),
  maximize(pearson, low = 0, high = 1),
  maximize(imp_rf),
  maximize(infogain)
)

num_selected(
  scores_combined,
  num_terms = 2,
  maximize(pval),
  maximize(pearson, low = 0, high = 1),
  maximize(imp_rf),
  maximize(infogain),
)

num_selected(
  scores_combined,
  maximize(pearson)
)
