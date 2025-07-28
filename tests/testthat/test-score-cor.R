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
  perm_pearson_res <-
    score_cor_pearson |>
    fit(mpg ~ ., data = mtcars)

  perm_spearman_res <-
    score_cor_spearman |>
    fit(mpg ~ ., data = mtcars)

  # ----------------------------------------------------------------------------

  predictors <- perm_pearson_res@results$predictor
  for (predictor in predictors) {
    pearson <- perm_pearson_res@results[
      perm_pearson_res@results$predictor == predictor,
    ]
    spearman <- perm_spearman_res@results[
      perm_spearman_res@results$predictor == predictor,
    ]

    tmp_data <- tibble::tibble(x = mtcars[[predictor]], y = mtcars$mpg)
    pearson_exp <- stats::cor(tmp_data$x, tmp_data$y, method = "pearson")
    spearman_exp <- stats::cor(tmp_data$x, tmp_data$y, method = "spearman")

    expect_equal(pearson$score, pearson_exp)
    expect_equal(spearman$score, spearman_exp)
  }

  # ----------------------------------------------------------------------------

  expect_equal(perm_pearson_res@range, c(-1.0, 1.0))
  expect_equal(perm_pearson_res@inclusive, rep(TRUE, 2))
  expect_equal(perm_pearson_res@fallback_value, 1)
  expect_equal(perm_pearson_res@direction, "maximize")
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

skip()

test_that("Pearson correlation filters", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()

  res_1 <-
    score_cor_pearson |>
    fit(Sale_Price ~ ., data = ames_subset)
  res_1 <- res_1@results

  expect_true(tibble::is_tibble(res_1))

  expect_identical(nrow(res_1), ncol(ames_subset) - 1L)

  expect_named(res_1, c("name", "score", "outcome", "predictor"))

  expect_equal(unique(res_1$name), "cor_pearson")

  expect_equal(unique(res_1$outcome), "Sale_Price")

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
    res_1$score,
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
  # missing values

  ames_missing <- ames_subset
  ames_missing$Sale_Price[1] <- NA_real_
  ames_missing$Lot_Frontage[2] <- NA_real_

  res_1 <-
    score_cor_pearson |>
    fit(Sale_Price ~ ., data = ames_subset)
  res_1 <- res_1@results

  expect_true(tibble::is_tibble(res_1))

  expect_identical(nrow(res_1), ncol(ames_subset) - 1L)

  expect_named(res_1, c("name", "score", "outcome", "predictor"))

  expect_equal(unique(res_1$name), "cor_pearson")

  expect_equal(unique(res_1$outcome), "Sale_Price")

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
    res_1$score,
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

  res_3 <-
    score_cor_pearson |>
    fit(Sale_Price ~ ., data = ames_subset, case_weights = two_weights)
  res_3 <- res_3@results

  expect_true(tibble::is_tibble(res_3))

  expect_identical(nrow(res_3), ncol(ames_subset) - 1L)

  expect_named(res_3, c("name", "score", "outcome", "predictor"))

  expect_equal(unique(res_3$name), "cor_pearson")

  expect_equal(unique(res_3$outcome), "Sale_Price")

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
    res_3$score,
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

test_that("Spearman correlation filters", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()

  res_1 <-
    score_cor_spearman |>
    fit(Sale_Price ~ ., data = ames_subset)
  res_1 <- res_1@results

  expect_true(tibble::is_tibble(res_1))

  expect_identical(nrow(res_1), ncol(ames_subset) - 1L)

  expect_named(res_1, c("name", "score", "outcome", "predictor"))

  expect_equal(unique(res_1$name), "cor_spearman")

  expect_equal(unique(res_1$outcome), "Sale_Price")

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
    res_1$score,
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
  # missing values

  ames_missing <- ames_subset
  ames_missing$Sale_Price[1] <- NA_real_
  ames_missing$Lot_Frontage[2] <- NA_real_

  res_1 <-
    score_cor_spearman |>
    fit(Sale_Price ~ ., data = ames_subset)
  res_1 <- res_1@results

  expect_true(tibble::is_tibble(res_1))

  expect_identical(nrow(res_1), ncol(ames_subset) - 1L)

  expect_named(res_1, c("name", "score", "outcome", "predictor"))

  expect_equal(unique(res_1$name), "cor_spearman")

  expect_equal(unique(res_1$outcome), "Sale_Price")

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
    res_1$score,
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

  res_3 <-
    score_cor_spearman |>
    fit(Sale_Price ~ ., data = ames_subset, case_weights = two_weights)
  res_3 <- res_3@results

  expect_true(tibble::is_tibble(res_3))

  expect_identical(nrow(res_3), ncol(ames_subset) - 1L)

  expect_named(res_3, c("name", "score", "outcome", "predictor"))

  expect_equal(unique(res_3$name), "cor_spearman")

  expect_equal(unique(res_3$outcome), "Sale_Price")

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
    res_3$score,
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
})
