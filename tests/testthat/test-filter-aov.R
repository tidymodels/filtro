test_that("get_scores_aov() is working", {
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
  score_obj = score_aov()
  res <- get_scores_aov(score_obj, data, outcome)

  fit <- lm(ames$Sale_Price ~ ames$MS_SubClass)
  exp.MS_SubClass <- anova(fit)$`F value`[1]

  fit <- lm(ames$Sale_Price ~ ames$MS_Zoning)
  exp.MS_Zoning <- anova(fit)$`F value`[1]

  exp.Lot_Frontage <- NA

  exp.Lot_Area <- NA

  fit <- lm(ames$Sale_Price ~ ames$Street)
  exp.Street <- anova(fit)$`F value`[1]

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

  expect_equal(unique(res$name), "fstat")

  expect_equal(unique(res$outcome), "Sale_Price")
})

# TODO Test Reversed lm(x ~ y)
# TODO Test pval
# TODO Test more after we add validators
