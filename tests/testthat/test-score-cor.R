test_that("get_scores_cor() is working for pearson", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()
  score_obj <- score_cor()
  score_res <- get_scores_cor(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )

  expect_true(tibble::is_tibble(score_res))

  expect_identical(nrow(score_res), ncol(ames_subset) - 1L)

  expect_named(score_res, c("name", "score", "outcome", "predictor"))

  expect_equal(unique(score_res$name), "pearson")

  expect_equal(unique(score_res$outcome), "Sale_Price")

  exp.MS_SubClass <- NA

  exp.MS_Zoning <- NA

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
    score_res$score,
    c(
      exp.MS_SubClass,
      exp.MS_Zoning,
      exp.Lot_Frontage,
      exp.Lot_Area,
      exp.Street
    )
  )
})

# TODO Test Reversed stats::lm(x ~ y)

test_that("get_scores_cor() is working for spearman", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()
  score_obj <- score_cor(score_type = "spearman")
  score_res <- get_scores_cor(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )

  expect_true(tibble::is_tibble(score_res))

  expect_identical(nrow(score_res), ncol(ames_subset) - 1L)

  expect_named(score_res, c("name", "score", "outcome", "predictor"))

  expect_equal(unique(score_res$name), "spearman")

  expect_equal(unique(score_res$outcome), "Sale_Price")

  exp.MS_SubClass <- NA

  exp.MS_Zoning <- NA

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
    score_res$score,
    c(
      exp.MS_SubClass,
      exp.MS_Zoning,
      exp.Lot_Frontage,
      exp.Lot_Area,
      exp.Street
    )
  )
})

# TODO Test more after we add validators
