test_that("attach_score() is working for aov", {
  ames_subset <- helper_ames()
  ames_subset <- ames_subset |>
    dplyr::mutate(Sale_Price = log10(Sale_Price))

  score_obj <- filtro::score_aov()
  score_res <- filtro::get_scores_aov(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )
  ex.score_obj <- score_obj |> attach_score(score_res)

  expect_equal(ex.score_obj@score_res, score_res)
})

test_that("arrange_score() is working for aov", {
  ames_subset <- helper_ames()
  ames_subset <- ames_subset |>
    dplyr::mutate(Sale_Price = log10(Sale_Price))

  score_obj <- filtro::score_aov(direction = "maximize") # TODO Right now user have to run > 3 lines to re-arrange score
  score_res <- filtro::get_scores_aov(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )
  ex.max <- score_obj |>
    filtro::attach_score(score_res = score_res) |>
    filtro::arrange_score()

  score_obj <- filtro::score_aov(direction = "minimize")
  score_res <- filtro::get_scores_aov(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )
  ex.min <- score_obj |>
    filtro::attach_score(score_res = score_res) |>
    filtro::arrange_score()

  score_obj <- filtro::score_aov(direction = "target")
  score_res <- filtro::get_scores_aov(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )
  ex.target <- score_obj |>
    filtro::attach_score(score_res = score_res) |>
    filtro::arrange_score(target = 94.4)

  score_obj <- filtro::score_aov(direction = "target")
  score_res <- filtro::get_scores_aov(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )
  ex.target2 <- score_obj |>
    filtro::attach_score(score_res = score_res) |>
    filtro::arrange_score(target = 22.8)

  expect_equal(ex.max, score_res |> dplyr::arrange(desc(score)))

  expect_equal(ex.min, score_res |> dplyr::arrange(score))

  expect_equal(ex.target, score_res |> dplyr::arrange(abs(score - 94.4)))

  expect_equal(ex.target2, score_res |> dplyr::arrange(abs(score - 22.8)))
})

skip()

# test_that("trans_score() is working for aov", {
#   # skip_if_not_installed("modeldata")
#   # data(ames, package = "modeldata")
#   # data <- modeldata::ames |>
#   #   dplyr::select(
#   #     Sale_Price,
#   #     MS_SubClass,
#   #     MS_Zoning,
#   #     Lot_Frontage,
#   #     Lot_Area,
#   #     Street
#   #   )
#   # outcome <- "Sale_Price"
#   # score_obj = score_aov()
#   # score_res <- get_scores_aov(score_obj, data, outcome)
#   # score_obj <- score_obj |> attach_score(score_res)

#   score_obj <- ames_score_obj()
#   score_res <- score_obj$score_res

#   score_obj$trans <- NULL # Default
#   ex.identity <- score_obj |> trans_score()

#   score_obj$trans <- scales::transform_log()
#   ex.log <- score_obj |> trans_score()

#   # TODO Add a couple more scales::transform_*()

#   expect_equal(ex.identity, score_res)

#   expect_equal(ex.log, score_res |> dplyr::mutate(score = log(score)))
# })

test_that("filter_score_num() is working for aov", {
  # skip_if_not_installed("modeldata")
  # data(ames, package = "modeldata")
  # data <- modeldata::ames |>
  #   dplyr::select(
  #     Sale_Price,
  #     MS_SubClass,
  #     MS_Zoning,
  #     Lot_Frontage,
  #     Lot_Area,
  #     Street
  #   )
  # outcome <- "Sale_Price"
  # score_obj = score_aov()
  # score_res <- get_scores_aov(score_obj, data, outcome)
  # score_obj <- score_obj |> attach_score(score_res)

  score_obj <- ames_score_obj()
  score_res <- score_obj$score_res

  score_obj$direction <- "maximize" # Default
  ex.max <- score_obj |> filter_score_num(num_terms = 2)

  score_obj$direction <- "minimize"
  ex.min <- score_obj |> filter_score_num(num_terms = 2)

  score_obj$direction <- "target"
  ex.target <- score_obj |>
    filter_score_num(score_obj, num_terms = 2, target = 63.8)

  score_obj$direction <- "target"
  ex.target2 <- score_obj |>
    filter_score_num(score_obj, num_terms = 2, target = 10.4)

  expect_equal(ex.max, score_res |> dplyr::slice_max(score, n = 2))

  expect_equal(ex.min, score_res |> dplyr::slice_min(score, n = 2))

  expect_equal(
    ex.target,
    score_res |>
      dplyr::arrange(abs(score - 63.8)) |>
      dplyr::slice_head(n = 2)
  )

  expect_equal(
    ex.target2,
    score_res |>
      dplyr::arrange(abs(score - 10.4)) |>
      dplyr::slice_head(n = 2)
  )
})

test_that("filter_score_prop() is working for aov", {
  # skip_if_not_installed("modeldata")
  # data(ames, package = "modeldata")
  # data <- modeldata::ames |>
  #   dplyr::select(
  #     Sale_Price,
  #     MS_SubClass,
  #     MS_Zoning,
  #     Lot_Frontage,
  #     Lot_Area,
  #     Street
  #   )
  # outcome <- "Sale_Price"
  # score_obj = score_aov()
  # score_res <- get_scores_aov(score_obj, data, outcome)
  # score_obj <- score_obj |> attach_score(score_res)

  score_obj <- ames_score_obj()
  score_res <- score_obj$score_res

  score_obj$direction <- "maximize" # Default
  ex.max <- score_obj |> filter_score_prop(prop_terms = 0.2) # TODO Can return NULL for prop = 0.1 if # of predictor is small

  score_obj$direction <- "minimize"
  ex.min <- score_obj |> filter_score_prop(prop_terms = 0.2) # TODO Can return NULL for prop = 0.1 if # of predictor is small

  score_obj$direction <- "target"
  ex.target <- score_obj |>
    filter_score_prop(score_obj, prop_terms = 0.2, target = 63.8) # TODO Can return NULL for prop = 0.1 if # of predictor is small

  score_obj$direction <- "target"
  ex.target2 <- score_obj |>
    filter_score_prop(score_obj, prop_terms = 0.2, target = 10.4)

  expect_equal(ex.max, score_res |> dplyr::slice_max(score, prop = 0.2))

  expect_equal(ex.min, score_res |> dplyr::slice_min(score, prop = 0.2))

  expect_equal(
    ex.target,
    score_res |>
      dplyr::arrange(abs(score - 63.8)) |>
      dplyr::slice_head(prop = 0.2)
  )

  expect_equal(
    ex.target2,
    score_res |>
      dplyr::arrange(abs(score - 10.4)) |>
      dplyr::slice_head(prop = 0.2)
  )
})

test_that("filter_score_cutoff() is working for aov", {
  # skip_if_not_installed("modeldata")
  # data(ames, package = "modeldata")
  # data <- modeldata::ames |>
  #   dplyr::select(
  #     Sale_Price,
  #     MS_SubClass,
  #     MS_Zoning,
  #     Lot_Frontage,
  #     Lot_Area,
  #     Street
  #   )
  # outcome <- "Sale_Price"
  # score_obj = score_aov()
  # score_res <- get_scores_aov(score_obj, data, outcome)
  # score_obj <- score_obj |> attach_score(score_res)

  score_obj <- ames_score_obj()
  score_res <- score_obj$score_res

  score_obj$direction <- "maximize" # Default
  ex.max <- score_obj |> filter_score_cutoff(cutoff = 63.8)

  score_obj$direction <- "minimize"
  ex.min <- score_obj |> filter_score_cutoff(cutoff = 63.8)

  score_obj$direction <- "target"
  ex.target <- score_obj |> filter_score_cutoff(target = 63.8, cutoff = 4)

  score_obj$direction <- "target"
  ex.target2 <- score_obj |> filter_score_cutoff(target = 10.4, cutoff = 1)

  expect_equal(
    ex.max,
    score_res |> dplyr::filter(score >= 63.8)
  )

  expect_equal(
    ex.min,
    score_res |> dplyr::filter(score <= 63.8)
  )

  expect_equal(
    ex.target,
    score_res |> dplyr::filter(abs(score - 63.8) <= 4)
  )

  expect_equal(
    ex.target2,
    score_res |> dplyr::filter(abs(score - 10.4) <= 1)
  )
})

test_that("filter_score_auto() is working for aov", {
  # skip_if_not_installed("modeldata")
  # data(ames, package = "modeldata")
  # data <- modeldata::ames |>
  #   dplyr::select(
  #     Sale_Price,
  #     MS_SubClass,
  #     MS_Zoning,
  #     Lot_Frontage,
  #     Lot_Area,
  #     Street
  #   )
  # outcome <- "Sale_Price"
  # score_obj = score_aov()
  # score_res <- get_scores_aov(score_obj, data, outcome)
  # score_obj <- score_obj |> attach_score(score_res)

  score_obj <- ames_score_obj()
  score_res <- score_obj$score_res

  score_obj$direction <- "maximize"
  ex.max <- score_obj |> filter_score_auto(num_terms = 2)
  ex.max2 <- score_obj |> filter_score_auto(num_terms = 2, cutoff = 63.9)
  ex.max3 <- score_obj |> filter_score_auto(prop_terms = 0.5)
  ex.max4 <- score_obj |> filter_score_auto(prop_terms = 0.5, cutoff = 63.9)

  expect_equal(
    ex.max,
    score_obj |> filter_score_num(num_terms = 2)
  )

  expect_equal(
    ex.max2,
    {
      score_obj$score_res <- score_obj |> filter_score_num(num_terms = 2)
      score_obj |> filter_score_cutoff(cutoff = 63.9)
    }
  )

  # TODO Finish the rest
})

# TODO Test rank_score_min()

# TODO Test rank_score_dense()

# score_res %>%
#   dplyr::mutate(rank = dplyr::min_rank((dplyr::desc(score))))

# score_res %>%
#   dplyr::mutate(rank = dplyr::min_rank((score)))

# score_res %>%
#   dplyr::mutate(rank = dplyr::dense_rank((dplyr::desc(score))))

# score_res %>%
#   dplyr::mutate(rank = dplyr::dense_rank((score)))

# TODO Test bind_scores()

# TODO Test fill_safe_values()

# score_set <- score_obj_list |> bind_scores()
# for (i in 1:length(score_obj_list)) {
#   method_name <- score_obj_list[[i]]$score_type
#   fallback_val <- score_obj_list[[i]]$fallback_value
#   score_set[[method_name]][is.na(score_set[[method_name]])] <- fallback_val
# }
# score_set

# TODO May need to test for methods other than aov
