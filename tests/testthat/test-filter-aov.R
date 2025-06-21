test_that("get_score_roc_auc() is working", {
  data <- iris
  outcome <- "Species"
  score_obj = score_aov()
  res <- get_score_aov(score_obj, data, outcome)

  fit <- lm(iris$Sepal.Length ~ iris$Species) # Reversed
  exp.Sepal.Length <- anova(fit)$`F value`[1]

  fit <- lm(iris$Sepal.Width ~ iris$Species) # Reversed
  exp.Sepal.Width <- anova(fit)$`F value`[1]

  fit <- lm(iris$Petal.Length ~ iris$Species) # Reversed
  exp.Petal.Length <- anova(fit)$`F value`[1]

  fit <- lm(iris$Petal.Width ~ iris$Species) # Reversed
  exp.Petal.Width <- anova(fit)$`F value`[1]

  expect_true(tibble::is_tibble(res))

  expect_identical(nrow(res), ncol(iris) - 1L)

  expect_named(res, c("name", "score", "outcome", "predictor"))

  expect_identical(
    res$score,
    c(exp.Sepal.Length, exp.Sepal.Width, exp.Petal.Length, exp.Petal.Width)
  )
  # Test name is auc
  # Test outcome
})
