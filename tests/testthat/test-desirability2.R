# Either move this file to the desirability2 package and rename it to 'test-filtro.R',
# or keep it in the current location with the same filename.

ames_subset <- helper_ames()
ames_subset <- ames_subset |>
  dplyr::mutate(Sale_Price = log10(Sale_Price))

score_obj_aov <- filtro::score_aov()
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

mtr <- score_obj_list |> filtro::fill_safe_values()
mtr <- mtr |> # TODO Do not include outcome in @results from the very beginning
  dplyr::select(-outcome)

#pak::pak("tidymodels/desirability2")
library(desirability2)

all_vars <- names(mtr)

desirability2::desirability(maximize(fstat))

res <- desirability2::desirability(
  maximize(fstat),
  maximize(pearson),
  .use_data = TRUE
)

d_vars <- sort(unique(unlist(res@variables)))
