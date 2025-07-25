test_that("object creation", {
  expect_s3_class(
    score_roc_auc,
    c("filtro::class_score_roc_auc", "filtro::class_score", "S7_object")
  )
})

test_that("computations - class outcome", {
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

# regression task
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

ames_roc_auc_res <- score_roc_auc |>
  fit(Sale_Price ~ ., data = ames_subset)
ames_roc_auc_res@results

skip()

test_that("get_scores_roc_auc() is working", {
  skip_if_not_installed("modeldata")

  cells_subset <- modeldata::cells |>
    dplyr::select(
      class,
      angle_ch_1,
      area_ch_1,
      avg_inten_ch_1,
      avg_inten_ch_2
    )

  score_obj = score_roc_auc()
  score_res <- get_scores_roc_auc(
    score_obj,
    data = cells_subset,
    outcome = "class"
  )

  roc <- pROC::roc(
    cells_subset$class,
    cells_subset$angle_ch_1,
    direction = "auto",
    quiet = TRUE
  )
  exp.angle_ch_1 <- pROC::auc(roc) |> as.numeric()

  roc <- pROC::roc(
    cells_subset$class,
    cells_subset$area_ch_1,
    direction = "auto",
    quiet = TRUE
  )
  exp.area_ch_1 <- pROC::auc(roc) |> as.numeric()

  roc <- pROC::roc(
    cells_subset$class,
    cells_subset$avg_inten_ch_1,
    direction = "auto",
    quiet = TRUE
  )
  exp.avg_inten_ch_1 <- pROC::auc(roc) |> as.numeric()

  roc <- pROC::roc(
    cells_subset$class,
    cells_subset$avg_inten_ch_2,
    direction = "auto",
    quiet = TRUE
  )
  exp.avg_inten_ch_2 <- pROC::auc(roc) |> as.numeric()

  expect_true(tibble::is_tibble(score_res))

  expect_identical(nrow(score_res), ncol(cells_subset) - 1L)

  expect_named(score_res, c("name", "score", "outcome", "predictor"))

  expect_identical(
    score_res$score,
    c(
      exp.angle_ch_1,
      exp.area_ch_1,
      exp.avg_inten_ch_1,
      exp.avg_inten_ch_2
    )
  )

  expect_equal(unique(score_res$name), "roc_auc")

  expect_equal(unique(score_res$outcome), "class")
})

# TODO Test pROC::multiclass.roc
# TODO Test more after we add validators
