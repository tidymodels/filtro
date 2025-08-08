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

test_that("computations - numeric outcome", {
  skip_if_not_installed("modeldata")
  ames_subset <- helper_ames()
  ames_subset <- ames_subset |>
    dplyr::mutate(Sale_Price = log10(Sale_Price))

  regression_task <- score_info_gain
  ames_info_gain_res <- regression_task |>
    fit(Sale_Price ~ ., data = ames_subset)

  regression_task <- score_gain_ratio
  ames_gain_ratio_res <- regression_task |>
    fit(Sale_Price ~ ., data = ames_subset)

  regression_task <- score_sym_uncert
  ames_sym_uncert_res <- regression_task |>
    fit(Sale_Price ~ ., data = ames_subset)

  # ----------------------------------------------------------------------------

  y <- ames_subset[["Sale_Price"]]
  X <- ames_subset[setdiff(names(ames_subset), "Sale_Price")]
  fit_infogain <- FSelectorRcpp::information_gain(
    x = X,
    y = y,
    type = "infogain",
    equal = TRUE
  )
  imp_infogain <- fit_infogain$importance
  imp_infogain[is.na(imp_infogain)] <- 0

  fit_gainratio <- FSelectorRcpp::information_gain(
    x = X,
    y = y,
    type = "gainratio",
    equal = TRUE
  )
  imp_gainratio <- fit_gainratio$importance
  imp_gainratio[is.na(imp_gainratio)] <- 0

  fit_symuncert <- FSelectorRcpp::information_gain(
    x = X,
    y = y,
    type = "symuncert",
    equal = TRUE
  )
  imp_symuncert <- fit_symuncert$importance
  imp_symuncert[is.na(imp_symuncert)] <- 0

  expect_equal(ames_info_gain_res@results$score, imp_infogain)
  expect_equal(ames_gain_ratio_res@results$score, imp_gainratio)
  expect_equal(ames_sym_uncert_res@results$score, imp_symuncert)

  # ----------------------------------------------------------------------------

  expect_equal(ames_info_gain_res@range, c(0.0, Inf))
  expect_equal(ames_info_gain_res@inclusive, rep(FALSE, 2))
  expect_equal(ames_info_gain_res@fallback_value, Inf)
  expect_equal(ames_info_gain_res@direction, "maximize")

  # ----------------------------------------------------------------------------

  expect_equal(ames_gain_ratio_res@range, c(0.0, 1.0))
  expect_equal(ames_gain_ratio_res@inclusive, rep(TRUE, 2))
  expect_equal(ames_gain_ratio_res@fallback_value, 1.0)
  expect_equal(ames_gain_ratio_res@direction, "maximize")
})

# TODO computations - wrong variable types

test_that("computations - required packages", {
  expect_equal(required_pkgs(score_info_gain), "filtro")
  expect_equal(required_pkgs(score_gain_ratio), "filtro")
  expect_equal(required_pkgs(score_sym_uncert), "filtro")
})

# TODO Test more after we add validators
