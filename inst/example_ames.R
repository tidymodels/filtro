skip_if_not_installed("modeldata")
data(ames, package = "modeldata")
data <- modeldata::ames |>
  dplyr::select(
    Sale_Price,
    MS_SubClass,
    MS_Zoning,
    Lot_Frontage,
    Lot_Area,
    Street #,
    # Alley, # ADD MORE
    # Lot_Shape,
    # Land_Contour,
    # Utilities,
    # Lot_Config,
    # Land_Slope
  )
data <- data |>
  dplyr::mutate(Sale_Price = log10(Sale_Price))
outcome <- "Sale_Price"
score_obj = filtro::score_aov()
score_res <- filtro::get_scores_aov(score_obj, data, outcome)

# Attach score
score_obj <- score_obj |> filtro::attach_score(score_res)
score_obj$score_res

# Arrange score
score_obj$direction <- "maximize" # Default
score_obj |> arrange_score()

score_obj$direction <- "minimize"
score_obj |> arrange_score()

score_obj$direction <- "target"
score_obj |> arrange_score(target = 63.8)

# Transform score
score_obj$trans <- NULL # Default
score_obj |> trans_score()

score_obj$trans <- scales::transform_log()
score_obj |> trans_score()

# Filter score based on number of predictors
score_obj$direction <- "maximize" # Default
score_obj |> filter_score_num(num_terms = 2)

score_obj$direction <- "minimize"
score_obj |> filter_score_num(num_terms = 2)

score_obj$direction <- "target"
score_obj |>
  filter_score_num(score_obj, num_terms = 2, target = 63.8)


# Filter score based on proportion of predictors
score_obj$direction <- "maximize" # Default
score_obj |> filter_score_prop(prop_terms = 0.2) # TODO Can return NULL for prop = 0.1 if # of predictor is small. dplyr::near()?

score_obj$direction <- "minimize"
score_obj |> filter_score_prop(prop_terms = 0.2) # TODO Can return NULL for prop = 0.1 if # of predictor is small. dplyr::near()?

score_obj$direction <- "target"
score_obj |>
  filter_score_prop(score_obj, prop_terms = 0.2, target = 63.8) # TODO Can return NULL for prop = 0.1 if # of predictor is small

# Filter score based on cutoff value
score_obj$direction <- "maximize"
score_obj |> filter_score_cutoff(cutoff = 63.8)

score_obj$direction <- "minimize"
score_obj |> filter_score_cutoff(cutoff = 63.8)

score_obj$direction <- "target"
score_obj |> filter_score_cutoff(cutoff = 4, target = 63.8) # TODO This cutoff value is based on abs(score - target). Not ideal?

# Filter score based on type and optional cutoff
score_obj$direction <- "maximize"
score_obj |> filter_score_auto(num_terms = 2)
score_obj |> filter_score_auto(num_terms = 2, cutoff = 63.9)
score_obj |> filter_score_auto(prop_terms = 0.5)
score_obj |> filter_score_auto(prop_terms = 0.5, cutoff = 63.9)

score_obj$direction <- "minimize"
score_obj |> filter_score_auto(num_terms = 2)
score_obj |> filter_score_auto(num_terms = 2, cutoff = 63.7)
score_obj |> filter_score_auto(prop_terms = 0.5)
score_obj |> filter_score_auto(prop_terms = 0.5, cutoff = 63.7)

score_obj$direction <- "target"
score_obj |> filter_score_auto(num_terms = 2, target = 63.8)
score_obj |> filter_score_auto(num_terms = 2, cutoff = 0.1, target = 63.8)
score_obj |> filter_score_auto(prop_terms = 0.5, target = 63.8)
score_obj |> filter_score_auto(prop_terms = 0.5, cutoff = 0.1, target = 63.8)

# Rank score based on min_rank
score_obj$direction <- "maximize"
score_obj |> rank_score_min()

score_obj$direction <- "minimize"
score_obj |> rank_score_min()

# Rank score based on dense_rank
score_obj$direction <- "maximize"
score_obj |> rank_score_dense()

score_obj$direction <- "minimize"
score_obj |> rank_score_dense()

# # Assign class result_obj to score object score_obj TODO
# score |> class()
# tmp <- score_obj |> as_result_obj(score)
# tmp$score |> class()

data(ames, package = "modeldata")
data <- modeldata::ames |>
  dplyr::select(
    Sale_Price,
    MS_SubClass,
    MS_Zoning,
    Lot_Frontage,
    Lot_Area,
    Street #,
    # Alley, # ADD MORE
    # Lot_Shape,
    # Land_Contour,
    # Utilities,
    # Lot_Config,
    # Land_Slope
  )
data <- data |>
  dplyr::mutate(Sale_Price = log10(Sale_Price))
outcome <- "Sale_Price"
score_obj = filtro::score_aov()
score_res <- filtro::get_scores_aov(score_obj, data, outcome)

# Bind scores and assign class
score_obj_aov <- filtro::score_aov()
score_res_aov <- filtro::get_scores_aov(score_obj_aov, data, outcome)
score_obj_aov <- score_obj_aov |> filtro::attach_score(score_res_aov)

score_obj_cor <- filtro::score_cor()
score_res_cor <- filtro::get_scores_cor(score_obj_cor, data, outcome)
score_obj_cor <- score_obj_cor |> filtro::attach_score(score_res_cor)

score_obj_imp <- filtro::score_forest_imp()
score_obj_imp$engine <- "ranger"
score_obj_imp$trees <- 10
score_obj_imp$mtry <- 2
score_obj_imp$min_n <- 1
score_obj_imp$class <- FALSE # TODO
set.seed(42)
score_res_imp <- filtro::get_scores_forest_importance(
  score_obj_imp,
  data,
  outcome
)
score_obj_imp <- score_obj_imp |> filtro::attach_score(score_res_imp)

score_obj_info <- filtro::score_info_gain()
score_obj_info$equal <- TRUE
score_res_info <- filtro::get_scores_info_gain(score_obj_info, data, outcome)
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
