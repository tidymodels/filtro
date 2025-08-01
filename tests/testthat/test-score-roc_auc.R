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

test_that("computations - class outcome, multiclass", {
  skip_if_not_installed("modeldata")
  # Avoid dealing with NA in pROC::multiclass.roc(); Somehow try() doesn't quite work.
  hpc_data <- helper_hpc_data() |> dplyr::select(-protocol)

  hpc_roc_auc_res <- score_roc_auc |>
    fit(class ~ ., data = hpc_data)

  # ----------------------------------------------------------------------------

  predictors <- hpc_roc_auc_res@results$predictor
  for (predictor in predictors) {
    tmp_data <- tibble::tibble(
      y = hpc_data$class,
      x = hpc_data[[predictor]]
    )
    roc <- pROC::multiclass.roc(
      tmp_data$y,
      tmp_data$x,
      direction = "auto",
      quiet = TRUE
    )
    fit_roc_auc <- pROC::auc(roc) |> as.numeric()

    roc_auc <- hpc_roc_auc_res@results[
      hpc_roc_auc_res@results$predictor == predictor,
    ]

    expect_equal(roc_auc$score, fit_roc_auc)
  }

  # ----------------------------------------------------------------------------

  expect_equal(hpc_roc_auc_res@range, c(0.0, 1.0))
  expect_equal(hpc_roc_auc_res@inclusive, rep(TRUE, 2))
  expect_equal(hpc_roc_auc_res@fallback_value, 1.0)
  expect_equal(hpc_roc_auc_res@direction, "maximize")
})

test_that("computations - numeric outcome, binary and multiclass predictors", {
  skip_if_not_installed("modeldata")
  # Avoid dealing with NA in pROC::multiclass.roc(); Somehow try() doesn't quite work.
  ames_subset <- helper_ames() |> dplyr::select(-Lot_Frontage, -Lot_Area)
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
    if (length(levels(tmp_data$y)) == 2) {
      roc <- pROC::roc(
        tmp_data$y,
        tmp_data$x,
        direction = "auto",
        quiet = TRUE
      )
    } else {
      roc <- pROC::multiclass.roc(
        tmp_data$y,
        tmp_data$x,
        direction = "auto",
        quiet = TRUE
      )
    }
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
