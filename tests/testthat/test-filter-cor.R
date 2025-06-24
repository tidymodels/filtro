test_that("get_scores_cor() is working", {
  skip_if_not_installed("modeldata")
  data(ames, package = "modeldata")
  data <- tibble::tibble(
    Sale_Price = ames$Sale_Price,
    MS_SubClass = ames$MS_SubClass,
    MS_Zoning = ames$MS_Zoning,
    Lot_Frontage = ames$Lot_Frontage,
    Lot_Area = ames$Lot_Area,
    Street = ames$Street,
  )
  outcome <- "Sale_Price"
  score_obj = score_cor()
  res <- get_scores_cor(score_obj, data, outcome)

  exp.MS_SubClass <- NA

  exp.MS_Zoning <- NA

  exp.Lot_Frontage <- cor(
    ames$Lot_Frontage,
    ames$Sale_Price,
    method = "pearson"
  )

  exp.Lot_Area <- cor(
    ames$Lot_Area,
    ames$Sale_Price,
    method = "pearson"
  )

  exp.Street <- NA

  expect_true(tibble::is_tibble(res))

  expect_identical(nrow(res), ncol(data) - 1L)

  expect_named(res, c("name", "score", "outcome", "predictor"))

  expect_identical(
    res$score,
    c(
      exp.MS_SubClass,
      exp.MS_Zoning,
      exp.Lot_Frontage,
      exp.Lot_Area,
      exp.Street
    )
  )

  expect_equal(unique(res$name), "pearson")

  expect_equal(unique(res$outcome), "Sale_Price")
})

# TODO Test Reversed stats::lm(x ~ y)
# TODO Test spearman
# TODO Test more after we add validators
