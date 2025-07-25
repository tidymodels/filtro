test_that("object creation", {
  expect_s3_class(
    score_roc_auc,
    c("filtro::class_score_roc_auc", "filtro::class_score", "S7_object")
  )
})

test_that("computations - class outcome, binary", {
  skip_if_not_installed("modeldata")
  cells_subset <- helper_cells()

  cells_roc_auc_res <- score_roc_auc |>
    fit(class ~ ., data = cells_subset)

  # ----------------------------------------------------------------------------

  predictors <- cells_roc_auc_res@results$predictor
  for (predictor in predictors) {
    tmp_data <- tibble::tibble(
      y = cells_subset$class,
      x = cells_subset[[predictor]]
    )
    roc <- pROC::roc(
      tmp_data$y,
      tmp_data$x,
      direction = "auto",
      quiet = TRUE
    )
    fit_roc_auc <- pROC::auc(roc) |> as.numeric()

    roc_auc <- cells_roc_auc_res@results[
      cells_roc_auc_res@results$predictor == predictor,
    ]

    expect_equal(roc_auc$score, fit_roc_auc)
  }

  # ----------------------------------------------------------------------------

  expect_equal(cells_roc_auc_res@range, c(0.0, 1.0))
  expect_equal(cells_roc_auc_res@inclusive, rep(TRUE, 2))
  expect_equal(cells_roc_auc_res@fallback_value, 1.0)
  expect_equal(cells_roc_auc_res@direction, "maximize")
})


test_that("computations - numeric outcome, multiclass predictors", {
  skip_if_not_installed("modeldata")
  ames_subset <- helper_ames() |> dplyr::select(-Lot_Frontage, -Lot_Area) # Avoid dealing with NA here
  ames_subset <- ames_subset |>
    dplyr::mutate(Sale_Price = log10(Sale_Price))

  ames_roc_auc_res <- score_roc_auc |>
    fit(Sale_Price ~ ., data = ames_subset)

  # ----------------------------------------------------------------------------

  predictors <- ames_roc_auc_res@results$predictor
  for (predictor in predictors) {
    tmp_data <- tibble::tibble(
      y = ames_subset[[predictor]],
      x = ames_subset$Sale_Price
    )
    roc <- pROC::multiclass.roc(
      tmp_data$y,
      tmp_data$x,
      direction = "auto",
      quiet = TRUE
    )
    fit_roc_auc <- pROC::auc(roc) |> as.numeric()

    roc_auc <- ames_roc_auc_res@results[
      ames_roc_auc_res@results$predictor == predictor,
    ]

    expect_equal(roc_auc$score, fit_roc_auc)
  }

  # ----------------------------------------------------------------------------

  expect_equal(ames_roc_auc_res@range, c(0.0, 1.0))
  expect_equal(ames_roc_auc_res@inclusive, rep(TRUE, 2))
  expect_equal(ames_roc_auc_res@fallback_value, 1.0)
  expect_equal(ames_roc_auc_res@direction, "maximize")
})

# TODO computations - wrong variable types

test_that("computations - required packages", {
  expect_equal(required_pkgs(score_roc_auc), "filtro")
})

# TODO Test more after we add validators
