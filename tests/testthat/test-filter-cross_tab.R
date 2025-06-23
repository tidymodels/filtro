test_that("get_score_cross_tab is working for chisq", {
  library(titanic)
  titanic_train$Survived <- titanic_train$Survived |> as.factor()
  titanic_train$Pclass <- titanic_train$Pclass |> as.factor()
  titanic_train$Sex <- titanic_train$Sex |> as.factor()
  titanic_train$Embarked <- titanic_train$Embarked |> as.factor()
  data <- tibble(
    Survived = titanic_train$Survived,
    Pclass = titanic_train$Pclass,
    Sex = titanic_train$Sex,
    Age = titanic_train$Age,
    Fare = titanic_train$Fare,
    Embarked = titanic_train$Embarked
  )
  outcome <- "Survived"
  score_obj <- score_cross_tab()
  filter_obj$score_type <- "chisq"
  filter_obj$fdr <- FALSE
  res <- get_score_cross_tab(filter_obj, data, outcome)

  exp.Pclass <- get_chisq(data$Pclass, data$Survived)
  exp.Sex <- get_chisq(data$Sex, data$Survived)
  exp.Age <- NA
  exp.Fare <- NA
  exp.Embarked <- get_chisq(data$Embarked, data$Survived)

  expect_true(tibble::is_tibble(res))

  expect_identical(nrow(res), ncol(data) - 1L)

  expect_named(res, c("name", "score", "outcome", "predictor"))

  expect_identical(
    res$score,
    c(exp.Pclass, exp.Sex, exp.Age, exp.Fare, exp.Embarked)
  )

  expect_equal(unique(res$name), "chisq")

  expect_equal(unique(res$outcome), "Survived")
})


test_that("get_score_cross_tab is working for fisher", {
  library(titanic)
  titanic_train$Survived <- titanic_train$Survived |> as.factor()
  titanic_train$Pclass <- titanic_train$Pclass |> as.factor()
  titanic_train$Sex <- titanic_train$Sex |> as.factor()
  titanic_train$Embarked <- titanic_train$Embarked |> as.factor()
  data <- tibble(
    Survived = titanic_train$Survived,
    Pclass = titanic_train$Pclass,
    Sex = titanic_train$Sex,
    Age = titanic_train$Age,
    Fare = titanic_train$Fare,
    Embarked = titanic_train$Embarked
  )
  outcome <- "Survived"
  score_obj <- score_cross_tab()
  filter_obj$score_type <- "fisher"
  filter_obj$fdr <- FALSE
  res <- get_score_cross_tab(filter_obj, data, outcome)

  exp.Pclass <- get_fisher(data$Pclass, data$Survived)
  exp.Sex <- get_fisher(data$Sex, data$Survived)
  exp.Age <- NA
  exp.Fare <- NA
  exp.Embarked <- get_fisher(data$Embarked, data$Survived)

  expect_true(tibble::is_tibble(res))

  expect_identical(nrow(res), ncol(data) - 1L)

  expect_named(res, c("name", "score", "outcome", "predictor"))

  expect_identical(
    res$score,
    c(exp.Pclass, exp.Sex, exp.Age, exp.Fare, exp.Embarked)
  )

  expect_equal(unique(res$name), "fisher")

  expect_equal(unique(res$outcome), "Survived")
})

# TODO Test fdr
# TODO Test more after we add validators
