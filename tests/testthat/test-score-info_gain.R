test_that("object creation", {
  expect_s3_class(
    score_info_gain,
    c("filtro::class_score_info_gain", "filtro::class_score", "S7_object")
  )

  expect_s3_class(
    score_gain_ratio,
    c("filtro::class_score_info_gain", "filtro::class_score", "S7_object")
  )

  expect_s3_class(
    score_sym_uncert,
    c("filtro::class_score_info_gain", "filtro::class_score", "S7_object")
  )
})

test_that("computations - class outcome", {
  skip_if_not_installed("modeldata")
  cells_subset <- helper_cells()

  cells_info_gain_res <- score_info_gain |>
    fit(class ~ ., data = cells_subset)

  cells_gain_ratio_res <- score_gain_ratio |>
    fit(class ~ ., data = cells_subset)

  cells_sym_uncert_res <- score_sym_uncert |>
    fit(class ~ ., data = cells_subset)

  # ----------------------------------------------------------------------------

  y <- cells_subset[["class"]]
  X <- cells_subset[setdiff(names(cells_subset), "class")]
  fit_infogain <- FSelectorRcpp::information_gain(
    x = X,
    y = y,
    type = "infogain"
  )
  imp_infogain <- fit_infogain$importance
  imp_infogain[is.na(imp_infogain)] <- 0

  fit_gainratio <- FSelectorRcpp::information_gain(
    x = X,
    y = y,
    type = "gainratio"
  )
  imp_gainratio <- fit_gainratio$importance
  imp_gainratio[is.na(imp_gainratio)] <- 0

  fit_symuncert <- FSelectorRcpp::information_gain(
    x = X,
    y = y,
    type = "symuncert"
  )
  imp_symuncert <- fit_symuncert$importance
  imp_symuncert[is.na(imp_symuncert)] <- 0

  expect_equal(cells_info_gain_res@results$score, imp_infogain)
  expect_equal(cells_gain_ratio_res@results$score, imp_gainratio)
  expect_equal(cells_sym_uncert_res@results$score, imp_symuncert)

  # ----------------------------------------------------------------------------

  expect_equal(cells_info_gain_res@range, c(0.0, Inf))
  expect_equal(cells_info_gain_res@inclusive, rep(FALSE, 2))
  expect_equal(cells_info_gain_res@fallback_value, Inf)
  expect_equal(cells_info_gain_res@direction, "maximize")

  # ----------------------------------------------------------------------------

  expect_equal(cells_gain_ratio_res@range, c(0.0, 1.0))
  expect_equal(cells_gain_ratio_res@inclusive, rep(TRUE, 2))
  expect_equal(cells_gain_ratio_res@fallback_value, 1.0)
  expect_equal(cells_gain_ratio_res@direction, "maximize")
})

# numeric outcome
ames_subset <- modeldata::ames |>
  dplyr::select(
    Sale_Price,
    MS_SubClass,
    MS_Zoning,
    Lot_Frontage,
    Lot_Area,
    Street
  )
ames_subset <- ames_subset |>
  dplyr::mutate(Sale_Price = log10(Sale_Price))

regression_task <- score_info_gain
regression_task@mode <- "regression"

ames_info_gain_regression_task_res <-
  regression_task |>
  fit(Sale_Price ~ ., data = ames_subset)
ames_info_gain_regression_task_res@results

skip()

test_that("get_scores_info_gain() is working for classification", {
  skip_if_not_installed("modeldata")

  cells_subset <- modeldata::cells |>
    dplyr::select(
      class,
      angle_ch_1,
      area_ch_1,
      avg_inten_ch_1,
      avg_inten_ch_2
    )

  score_obj <- score_info_gain()
  score_res <- get_scores_info_gain(
    score_obj,
    data = cells_subset,
    outcome = "class"
  )

  outcome <- "class"
  y <- cells_subset[[outcome]]
  X <- cells_subset[setdiff(names(cells_subset), outcome)]
  fit <- FSelectorRcpp::information_gain(x = X, y = y)
  exp.res <- fit$importance

  expect_true(tibble::is_tibble(score_res))

  expect_identical(nrow(score_res), ncol(cells_subset) - 1L)

  expect_named(score_res, c("name", "score", "outcome", "predictor"))

  expect_identical(score_res$score, exp.res)

  expect_equal(unique(score_res$name), "infogain")

  expect_equal(unique(score_res$outcome), "class")
})

test_that("get_scores_info_gain() is working for regression", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()
  score_obj <- score_info_gain(mode = "regression")
  score_res <- get_scores_info_gain(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )

  outcome <- "Sale_Price"
  y <- ames_subset[[outcome]]
  X <- ames_subset[setdiff(names(ames_subset), outcome)]
  fit <- FSelectorRcpp::information_gain(x = X, y = y, equal = TRUE)
  exp.res <- fit$importance

  expect_true(tibble::is_tibble(score_res))

  expect_identical(nrow(score_res), ncol(ames_subset) - 1L)

  expect_named(score_res, c("name", "score", "outcome", "predictor"))

  expect_identical(score_res$score, exp.res)

  expect_equal(unique(score_res$name), "infogain")

  expect_equal(unique(score_res$outcome), "Sale_Price")
})

# TODO Test more after we add validators
