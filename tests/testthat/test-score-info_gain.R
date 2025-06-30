test_that("get_scores_info_gain() is working for classification", {
  skip_if_not_installed("modeldata")
  data(cells, package = "modeldata")
  data <- modeldata::cells |>
    dplyr::select(
      case,
      class,
      angle_ch_1,
      area_ch_1,
      avg_inten_ch_1,
      avg_inten_ch_2
    )
  outcome <- "class"
  score_obj <- score_info_gain()
  score_obj$equal <- FALSE # Default
  score_res <- get_scores_info_gain(score_obj, data, outcome)

  y <- data[[outcome]]
  X <- data[setdiff(names(data), outcome)]
  fit <- FSelectorRcpp::information_gain(x = X, y = y)
  exp.res <- fit$importance

  expect_true(tibble::is_tibble(score_res))

  expect_identical(nrow(score_res), ncol(data) - 1L)

  expect_named(score_res, c("name", "score", "outcome", "predictor"))

  expect_identical(score_res$score, exp.res)

  expect_equal(unique(score_res$name), "infogain")

  expect_equal(unique(score_res$outcome), "class")
})

test_that("get_scores_info_gain() is working for regression", {
  skip_if_not_installed("modeldata")
  data(ames, package = "modeldata")
  data <- modeldata::ames |>
    dplyr::select(
      Sale_Price,
      MS_SubClass,
      MS_Zoning,
      Lot_Frontage,
      Lot_Area,
      Street
    )
  outcome <- "Sale_Price"
  score_obj <- score_info_gain()
  score_obj$equal <- TRUE # Set = TRUE for numeric outcome
  score_res <- get_scores_info_gain(score_obj, data, outcome)

  y <- data[[outcome]]
  X <- data[setdiff(names(data), outcome)]
  fit <- FSelectorRcpp::information_gain(x = X, y = y)
  exp.res <- fit$importance

  expect_true(tibble::is_tibble(score_res))

  expect_identical(nrow(score_res), ncol(data) - 1L)

  expect_named(score_res, c("name", "score", "outcome", "predictor"))

  expect_identical(score_res$score, exp.res)

  expect_equal(unique(score_res$name), "infogain")

  expect_equal(unique(score_res$outcome), "Sale_Price")
})

# TODO Test more after we add validators
