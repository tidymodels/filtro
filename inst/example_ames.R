skip_if_not_installed("modeldata")
data(ames, package = "modeldata")
data <- modeldata::ames |>
  dplyr::select(
    Sale_Price,
    MS_SubClass,
    MS_Zoning,
    Lot_Frontage,
    Lot_Area,
    Street,
    Alley, # ADD MORE
    Lot_Shape,
    Land_Contour,
    Utilities,
    Lot_Config,
    Land_Slope
  )
outcome <- "Sale_Price"
score_obj = score_aov()
res <- get_scores_aov(score_obj, data, outcome)

# Attach score
score_obj <- score_obj |> attach_score(res)
score_obj$res

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
score_obj |> filter_score_prop(prop_terms = 0.2) # TODO Can return NULL for prop = 0.1 if # of predictor is small

score_obj$direction <- "minimize"
score_obj |> filter_score_num(prop_terms = 0.2) # TODO Can return NULL for prop = 0.1 if # of predictor is small

score_obj$direction <- "target"
score_obj |>
  filter_score_num(score_obj, prop_terms = 0.2, target = 63.8) # TODO Can return NULL for prop = 0.1 if # of predictor is small

# Filter score based on cutoff value
