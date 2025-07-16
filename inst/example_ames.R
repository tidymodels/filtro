skip_if_not_installed("modeldata")

ames_subset <- helper_ames()
ames_subset <- ames_subset |>
  dplyr::mutate(Sale_Price = log10(Sale_Price))

score_obj <- filtro::score_aov()
score_res <- filtro::get_scores_aov(
  score_obj,
  data = ames_subset,
  outcome = "Sale_Price"
)

# Attach score
score_obj |> filtro::attach_score(score_res = score_res)
score_obj |> filtro::attach_score(score_res = "score_res") #TODO Add to test
potato <- S7::new_class("potato")
potato |> filtro::attach_score(score_res = score_res)

# Arrange score
score_obj <- filtro::score_aov(direction = "maximize")
score_res <- filtro::get_scores_aov(
  score_obj,
  data = ames_subset,
  outcome = "Sale_Price"
)
score_obj |>
  filtro::attach_score(score_res = score_res) |>
  filtro::arrange_score()

score_obj <- filtro::score_aov(direction = "minimize")
score_res <- filtro::get_scores_aov(
  score_obj,
  data = ames_subset,
  outcome = "Sale_Price"
)
score_obj |>
  filtro::attach_score(score_res = score_res) |>
  filtro::arrange_score()

score_obj <- filtro::score_aov(direction = "target")
score_res <- filtro::get_scores_aov(
  score_obj,
  data = ames_subset,
  outcome = "Sale_Price"
)
score_obj |>
  filtro::attach_score(score_res = score_res) |>
  filtro::arrange_score(target = 22.8)

score_obj <- filtro::score_aov(direction = "potato")
score_res <- filtro::get_scores_aov(
  score_obj,
  data = ames_subset,
  outcome = "Sale_Price"
)
score_obj |>
  filtro::attach_score(score_res = score_res) |>
  filtro::arrange_score()

# # Transform score
# score_obj@trans <- NULL # Default
# score_obj |> trans_score()

# score_obj@trans <- scales::transform_log()
# score_obj |> trans_score()

# Filter score based on number of predictors
score_obj <- filtro::score_aov(direction = "maximize")
score_res <- filtro::get_scores_aov(
  score_obj,
  data = ames_subset,
  outcome = "Sale_Price"
)
score_obj |>
  filtro::attach_score(score_res = score_res) |>
  filter_score_num(num_terms = 2)

score_obj <- filtro::score_aov(direction = "minimize")
score_res <- filtro::get_scores_aov(
  score_obj,
  data = ames_subset,
  outcome = "Sale_Price"
)
score_obj |>
  filtro::attach_score(score_res = score_res) |>
  filter_score_num(num_terms = 2)

score_obj <- filtro::score_aov(direction = "target")
score_res <- filtro::get_scores_aov(
  score_obj,
  data = ames_subset,
  outcome = "Sale_Price"
)
score_obj |>
  filtro::attach_score(score_res = score_res) |>
  filter_score_num(num_terms = 2, target = 94.4)

# Filter score based on proportion of predictors
score_obj <- filtro::score_aov(direction = "maximize")
score_res <- filtro::get_scores_aov(
  score_obj,
  data = ames_subset,
  outcome = "Sale_Price"
)
score_obj |>
  filtro::attach_score(score_res = score_res) |>
  filter_score_prop(prop_terms = 0.2)

score_obj <- filtro::score_aov(direction = "minimize")
score_res <- filtro::get_scores_aov(
  score_obj,
  data = ames_subset,
  outcome = "Sale_Price"
)
score_obj |>
  filtro::attach_score(score_res = score_res) |>
  filter_score_prop(prop_terms = 0.2)

score_obj <- filtro::score_aov(direction = "target")
score_res <- filtro::get_scores_aov(
  score_obj,
  data = ames_subset,
  outcome = "Sale_Price"
)
score_obj |>
  filtro::attach_score(score_res = score_res) |>
  filter_score_prop(prop_terms = 0.2, target = 94.4)

# Filter score based on cutoff value
score_obj <- filtro::score_aov(direction = "maximize")
score_res <- filtro::get_scores_aov(
  score_obj,
  data = ames_subset,
  outcome = "Sale_Price"
)
score_obj |>
  filtro::attach_score(score_res = score_res) |>
  filter_score_cutoff(cutoff = 94.4)

score_obj <- filtro::score_aov(direction = "minimize")
score_res <- filtro::get_scores_aov(
  score_obj,
  data = ames_subset,
  outcome = "Sale_Price"
)
score_obj |>
  filtro::attach_score(score_res = score_res) |>
  filter_score_cutoff(cutoff = 94.4)

score_obj <- filtro::score_aov(direction = "target")
score_res <- filtro::get_scores_aov(
  score_obj,
  data = ames_subset,
  outcome = "Sale_Price"
)
score_obj |>
  filtro::attach_score(score_res = score_res) |>
  filter_score_cutoff(cutoff = 4, target = 94.4)

# Filter score based on type and optional cutoff
# score_obj$direction <- "maximize"
# score_obj |> filter_score_auto(num_terms = 2)
# score_obj |> filter_score_auto(num_terms = 2, cutoff = 63.9)
# score_obj |> filter_score_auto(prop_terms = 0.5)
# score_obj |> filter_score_auto(prop_terms = 0.5, cutoff = 63.9)

# score_obj$direction <- "minimize"
# score_obj |> filter_score_auto(num_terms = 2)
# score_obj |> filter_score_auto(num_terms = 2, cutoff = 63.7)
# score_obj |> filter_score_auto(prop_terms = 0.5)
# score_obj |> filter_score_auto(prop_terms = 0.5, cutoff = 63.7)

# score_obj$direction <- "target"
# score_obj |> filter_score_auto(num_terms = 2, target = 63.8)
# score_obj |> filter_score_auto(num_terms = 2, cutoff = 0.1, target = 63.8)
# score_obj |> filter_score_auto(prop_terms = 0.5, target = 63.8)
# score_obj |> filter_score_auto(prop_terms = 0.5, cutoff = 0.1, target = 63.8)

# # Rank score based on min_rank
# score_obj$direction <- "maximize"
# score_obj |> rank_score_min()

# score_obj$direction <- "minimize"
# score_obj |> rank_score_min()

# # Rank score based on dense_rank
# score_obj$direction <- "maximize"
# score_obj |> rank_score_dense()

# score_obj$direction <- "minimize"
# score_obj |> rank_score_dense()

ames_subset <- helper_ames()
ames_subset <- ames_subset |>
  dplyr::mutate(Sale_Price = log10(Sale_Price))

# Bind scores and assign class
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

score_obj_imp <- filtro::score_forest_imp(is_reg = TRUE)
score_res_imp <- get_scores_forest_importance(
  score_obj_imp,
  data = ames_subset,
  outcome = "Sale_Price"
)
score_obj_imp <- score_obj_imp |> filtro::attach_score(score_res_imp)

score_obj_info <- score_info_gain(is_reg = TRUE)
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
) # TODO Right now user has to supply the list.
score_obj_list |> filtro::bind_scores()

# bind_scores(list())
# score_obj_list <- list(score_obj_aov)
# score_obj_list |> filtro::bind_scores()
# #score_obj_list <- list(score_obj_aov, score_obj_aov) # TODO

# Fill in safe values
score_obj_list <- list(
  score_obj_aov,
  score_obj_cor,
  score_obj_imp,
  score_obj_info
)
score_obj_list |> filtro::fill_safe_values()

# TODO Filter *

# TODO Drop outcome column
