test_that("get_scores_cor() is working", {
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
  score_obj = score_cor()
  score_res <- get_scores_cor(score_obj, data, outcome)

  exp.MS_SubClass <- NA

  exp.MS_Zoning <- NA

  exp.Lot_Frontage <- stats::cor(
    ames$Lot_Frontage,
    ames$Sale_Price,
    method = "pearson"
  )

  exp.Lot_Area <- stats::cor(
    ames$Lot_Area,
    ames$Sale_Price,
    method = "pearson"
  )

  exp.Street <- NA

  expect_true(tibble::is_tibble(score_res))

  expect_identical(nrow(score_res), ncol(data) - 1L)

  expect_named(score_res, c("name", "score", "outcome", "predictor"))

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

  expect_equal(unique(score_res$name), "pearson")

  expect_equal(unique(score_res$outcome), "Sale_Price")
})

# TODO Test Reversed stats::lm(x ~ y)
# TODO Test spearman
# TODO Test more after we add validators
