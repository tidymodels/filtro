test_that("object creation", {
  expect_s3_class(
    score_cor_pearson,
    c("filtro::class_score_cor", "filtro::class_score", "S7_object")
  )

  expect_s3_class(
    score_cor_spearman,
    c("filtro::class_score_cor", "filtro::class_score", "S7_object")
  )
})

test_that("computations", {
  mtcars_pearson_res <-
    score_cor_pearson |>
    fit(mpg ~ ., data = mtcars)

  mtcars_spearman_res <-
    score_cor_spearman |>
    fit(mpg ~ ., data = mtcars)

  # ----------------------------------------------------------------------------

  predictors <- mtcars_pearson_res@results$predictor
  for (predictor in predictors) {
    pearson <- mtcars_pearson_res@results[
      mtcars_pearson_res@results$predictor == predictor,
    ]
    spearman <- mtcars_spearman_res@results[
      mtcars_spearman_res@results$predictor == predictor,
    ]

    tmp_data <- tibble::tibble(x = mtcars[[predictor]], y = mtcars$mpg)
    pearson_exp <- stats::cor(tmp_data$x, tmp_data$y, method = "pearson")
    spearman_exp <- stats::cor(tmp_data$x, tmp_data$y, method = "spearman")

    expect_equal(pearson$score, pearson_exp)
    expect_equal(spearman$score, spearman_exp)
  }

  # ----------------------------------------------------------------------------

  expect_equal(mtcars_pearson_res@range, c(-1.0, 1.0))
  expect_equal(mtcars_pearson_res@inclusive, rep(TRUE, 2))
  expect_equal(mtcars_pearson_res@fallback_value, 1)
  expect_equal(mtcars_pearson_res@direction, "maximize")
})

test_that("computations - wrong variable types", {
  skip_if_not_installed("modeldata")

  perm_data <- helper_perm()
  perm_data <- perm_data |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("chem"), as.factor))

  perm_pearson_res <-
    score_cor_pearson |>
    fit(permeability ~ ., data = perm_data)

  expect_true(all(is.na(perm_pearson_res@results$score)))
})

test_that("computations - required packages", {
  expect_equal(required_pkgs(score_cor_pearson), "filtro")
  expect_equal(required_pkgs(score_cor_spearman), "filtro")
})

test_that("Pearson correlation filters - adding missing values and case weights", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()

  ames_pearson_res <-
    score_cor_pearson |>
    fit(Sale_Price ~ ., data = ames_subset)

  # ----------------------------------------------------------------------------

  predictors <- ames_pearson_res@results$predictor
  for (predictor in predictors) {
    pearson <- ames_pearson_res@results[
      ames_pearson_res@results$predictor == predictor,
    ]

    tmp_data <- tibble::tibble(
      x = ames_subset[[predictor]],
      y = ames_subset$Sale_Price
    )

    fit_pearson <- try(
      stats::cor(tmp_data$x, tmp_data$y, method = "pearson"),
      silent = TRUE
    )

    if (inherits(fit_pearson, "try-error")) {
      fit_pearson <- NA_real_
    }

    expect_equal(pearson$score, fit_pearson)
  }

  # ----------------------------------------------------------------------------
  # missing values

  ames_missing <- ames_subset
  ames_missing$Sale_Price[1] <- NA_real_
  ames_missing$Lot_Frontage[2] <- NA_real_

  ames_missing_pearson_res <-
    score_cor_pearson |>
    fit(Sale_Price ~ ., data = ames_missing)

  exp.MS_SubClass <- exp.MS_Zoning <- NA

  exp.Lot_Frontage <- stats::cor(
    ames_subset$Lot_Frontage,
    ames_subset$Sale_Price,
    method = "pearson"
  )

  exp.Lot_Area <- stats::cor(
    ames_subset$Lot_Area,
    ames_subset$Sale_Price,
    method = "pearson"
  )

  exp.Street <- NA

  expect_identical(
    ames_missing_pearson_res@results$score,
    c(
      exp.MS_SubClass,
      exp.MS_Zoning,
      exp.Lot_Frontage,
      exp.Lot_Area,
      exp.Street
    ),
    tolerance = 0.01
  )

  # ----------------------------------------------------------------------------
  # case weights

  two_weights <- c(1, 1, rep(0, nrow(ames_subset) - 2))

  ames_weights_pearson_res <-
    score_cor_pearson |>
    fit(Sale_Price ~ ., data = ames_subset, case_weights = two_weights)

  exp.MS_SubClass <- exp.MS_Zoning <- NA

  exp.Lot_Frontage <- stats::cor(
    ames_subset$Lot_Frontage[1:2],
    ames_subset$Sale_Price[1:2],
    method = "pearson"
  )

  exp.Lot_Area <- stats::cor(
    ames_subset$Lot_Area[1:2],
    ames_subset$Sale_Price[1:2],
    method = "pearson"
  )

  exp.Street <- NA

  expect_identical(
    ames_weights_pearson_res@results$score,
    c(
      exp.MS_SubClass,
      exp.MS_Zoning,
      exp.Lot_Frontage,
      exp.Lot_Area,
      exp.Street
    ),
    tolerance = 0.01
  )

  expect_snapshot(
    score_cor_pearson |>
      fit(Sale_Price ~ ., data = ames_subset, case_weights = 1),
    error = TRUE
  )

  expect_snapshot(
    score_cor_pearson |>
      fit(Sale_Price ~ ., data = ames_subset, case_weights = letters),
    error = TRUE
  )
})

test_that("Spearman correlation filters - adding missing values and case weights", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()

  ames_spearman_res <-
    score_cor_spearman |>
    fit(Sale_Price ~ ., data = ames_subset)

  # ----------------------------------------------------------------------------

  predictors <- ames_spearman_res@results$predictor
  for (predictor in predictors) {
    spearman <- ames_spearman_res@results[
      ames_spearman_res@results$predictor == predictor,
    ]

    tmp_data <- tibble::tibble(
      x = ames_subset[[predictor]],
      y = ames_subset$Sale_Price
    )

    fit_spearman <- try(
      stats::cor(tmp_data$x, tmp_data$y, method = "spearman"),
      silent = TRUE
    )

    if (inherits(fit_spearman, "try-error")) {
      fit_spearman <- NA_real_
    }

    expect_equal(spearman$score, fit_spearman)
  }

  # ----------------------------------------------------------------------------
  # missing values

  ames_missing <- ames_subset
  ames_missing$Sale_Price[1] <- NA_real_
  ames_missing$Lot_Frontage[2] <- NA_real_

  ames_missing_spearman_res <-
    score_cor_spearman |>
    fit(Sale_Price ~ ., data = ames_missing)

  exp.MS_SubClass <- exp.MS_Zoning <- NA

  exp.Lot_Frontage <- stats::cor(
    ames_subset$Lot_Frontage,
    ames_subset$Sale_Price,
    method = "spearman"
  )

  exp.Lot_Area <- stats::cor(
    ames_subset$Lot_Area,
    ames_subset$Sale_Price,
    method = "spearman"
  )

  exp.Street <- NA

  expect_identical(
    ames_missing_spearman_res@results$score,
    c(
      exp.MS_SubClass,
      exp.MS_Zoning,
      exp.Lot_Frontage,
      exp.Lot_Area,
      exp.Street
    ),
    tolerance = 0.01
  )

  # ----------------------------------------------------------------------------
  # case weights

  two_weights <- c(1, 1, rep(0, nrow(ames_subset) - 2))

  ames_weights_spearman_res <-
    score_cor_spearman |>
    fit(Sale_Price ~ ., data = ames_subset, case_weights = two_weights)

  exp.MS_SubClass <- exp.MS_Zoning <- NA

  exp.Lot_Frontage <- stats::cor(
    ames_subset$Lot_Frontage[1:2],
    ames_subset$Sale_Price[1:2],
    method = "spearman"
  )

  exp.Lot_Area <- stats::cor(
    ames_subset$Lot_Area[1:2],
    ames_subset$Sale_Price[1:2],
    method = "spearman"
  )

  exp.Street <- NA

  expect_identical(
    ames_weights_spearman_res@results$score,
    c(
      exp.MS_SubClass,
      exp.MS_Zoning,
      exp.Lot_Frontage,
      exp.Lot_Area,
      exp.Street
    ),
    tolerance = 0.01
  )

  expect_snapshot(
    score_cor_spearman |>
      fit(Sale_Price ~ ., data = ames_subset, case_weights = 1),
    error = TRUE
  )

  expect_snapshot(
    score_cor_spearman |>
      fit(Sale_Price ~ ., data = ames_subset, case_weights = letters),
    error = TRUE
  )
})

test_that("fit() works when the generic is available", {
  # https://github.com/tidymodels/filtro/pull/161

  sim_filter_1 <- score_cor_pearson |> fit(mpg ~ ., data = mtcars)
  suppressPackageStartupMessages(library(generics))
  sim_filter_2 <- score_cor_pearson |> fit(mpg ~ ., data = mtcars)
  expect_equal(sim_filter_1@results, sim_filter_2@results)
})
