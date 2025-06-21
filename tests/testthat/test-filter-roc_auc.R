test_that("get_score_roc_auc() is working", {
  data <- iris
  outcome <- "Species"
  score_obj = score_roc_auc()
  res <- get_score_roc_auc(score_obj, data, outcome)

  roc <- pROC::multiclass.roc(
    iris$Species,
    iris$Sepal.Length,
    direction = "auto",
    quiet = TRUE
  )
  exp.Sepal.Length <- pROC::auc(roc) |> as.numeric()

  roc <- pROC::multiclass.roc(
    iris$Species,
    iris$Sepal.Width,
    direction = "auto",
    quiet = TRUE
  )
  exp.Sepal.Width <- pROC::auc(roc) |> as.numeric()

  roc <- pROC::multiclass.roc(
    iris$Species,
    iris$Petal.Length,
    direction = "auto",
    quiet = TRUE
  )
  exp.Petal.Length <- pROC::auc(roc) |> as.numeric()

  roc <- pROC::multiclass.roc(
    iris$Species,
    iris$Petal.Width,
    direction = "auto",
    quiet = TRUE
  )
  exp.Petal.Width <- pROC::auc(roc) |> as.numeric()

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

# Test more after we add validators
