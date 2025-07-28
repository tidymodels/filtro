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
