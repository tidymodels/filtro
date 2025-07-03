test_that("get_scores_cross_tab is working for -log10(chisq pval)", {
  library(titanic)
  titanic_train <- titanic_train %>%
    dplyr::mutate(dplyr::across(c(Survived, Pclass, Sex, Embarked), as.factor))
  data <- titanic_train %>%
    dplyr::select(Survived, Pclass, Sex, Age, Fare, Embarked)
  outcome <- "Survived"
  score_obj <- score_cross_tab()
  score_obj$score_type <- "pval_chisq"
  score_obj$fdr <- FALSE
  score_res <- get_scores_cross_tab(score_obj, data, outcome)

  exp.Pclass <- get_single_chisq(data$Pclass, data$Survived)
  exp.Pclass <- -log10(exp.Pclass)
  exp.Sex <- get_single_chisq(data$Sex, data$Survived)
  exp.Sex <- -log10(exp.Sex)
  exp.Age <- NA
  exp.Fare <- NA
  exp.Embarked <- get_single_chisq(data$Embarked, data$Survived)
  exp.Embarked <- -log10(exp.Embarked)

  expect_true(tibble::is_tibble(score_res))

  expect_identical(nrow(score_res), ncol(data) - 1L)

  expect_named(score_res, c("name", "score", "outcome", "predictor"))

  expect_identical(
    score_res$score,
    c(exp.Pclass, exp.Sex, exp.Age, exp.Fare, exp.Embarked)
  )

  expect_equal(unique(score_res$name), "pval_chisq")

  expect_equal(unique(score_res$outcome), "Survived")
})

test_that("get_scores_cross_tab is working for chisq pval", {
  library(titanic)
  titanic_train <- titanic_train %>%
    dplyr::mutate(dplyr::across(c(Survived, Pclass, Sex, Embarked), as.factor))
  data <- titanic_train %>%
    dplyr::select(Survived, Pclass, Sex, Age, Fare, Embarked)
  outcome <- "Survived"
  score_obj <- score_cross_tab()
  score_obj$score_type <- "pval_chisq"
  score_obj$fdr <- FALSE
  score_obj$neg_log_pval <- FALSE # Turn -log10() off
  score_res <- get_scores_cross_tab(score_obj, data, outcome)

  exp.Pclass <- get_single_chisq(data$Pclass, data$Survived)
  exp.Sex <- get_single_chisq(data$Sex, data$Survived)
  exp.Age <- NA
  exp.Fare <- NA
  exp.Embarked <- get_single_chisq(data$Embarked, data$Survived)

  expect_true(tibble::is_tibble(score_res))

  expect_identical(nrow(score_res), ncol(data) - 1L)

  expect_named(score_res, c("name", "score", "outcome", "predictor"))

  expect_identical(
    score_res$score,
    c(exp.Pclass, exp.Sex, exp.Age, exp.Fare, exp.Embarked)
  )

  expect_equal(unique(score_res$name), "pval_chisq")

  expect_equal(unique(score_res$outcome), "Survived")
})

test_that("get_score_cross_tab is working for -log10(fisher pval)", {
  library(titanic)
  titanic_train <- titanic_train %>%
    dplyr::mutate(dplyr::across(c(Survived, Pclass, Sex, Embarked), as.factor))
  data <- titanic_train %>%
    dplyr::select(Survived, Pclass, Sex, Age, Fare, Embarked)
  outcome <- "Survived"
  score_obj <- score_cross_tab()
  score_obj$score_type <- "pval_fisher"
  score_obj$fdr <- FALSE
  score_res <- get_scores_cross_tab(score_obj, data, outcome)

  exp.Pclass <- get_single_fisher(data$Pclass, data$Survived)
  exp.Pclass <- -log10(exp.Pclass)
  exp.Sex <- get_single_fisher(data$Sex, data$Survived)
  exp.Sex <- -log10(exp.Sex)
  exp.Age <- NA
  exp.Fare <- NA
  exp.Embarked <- get_single_fisher(data$Embarked, data$Survived)
  exp.Embarked <- -log10(exp.Embarked)

  expect_true(tibble::is_tibble(score_res))

  expect_identical(nrow(score_res), ncol(data) - 1L)

  expect_named(score_res, c("name", "score", "outcome", "predictor"))

  expect_identical(
    score_res$score,
    c(exp.Pclass, exp.Sex, exp.Age, exp.Fare, exp.Embarked)
  )

  expect_equal(unique(score_res$name), "pval_fisher")

  expect_equal(unique(score_res$outcome), "Survived")
})

test_that("get_score_cross_tab is working for fisher pval", {
  library(titanic)
  titanic_train <- titanic_train %>%
    dplyr::mutate(dplyr::across(c(Survived, Pclass, Sex, Embarked), as.factor))
  data <- titanic_train %>%
    dplyr::select(Survived, Pclass, Sex, Age, Fare, Embarked)
  outcome <- "Survived"
  score_obj <- score_cross_tab()
  score_obj$score_type <- "pval_fisher"
  score_obj$fdr <- FALSE
  score_obj$neg_log_pval <- FALSE # Turn -log10() off
  score_res <- get_scores_cross_tab(score_obj, data, outcome)

  exp.Pclass <- get_single_fisher(data$Pclass, data$Survived)
  exp.Sex <- get_single_fisher(data$Sex, data$Survived)
  exp.Age <- NA
  exp.Fare <- NA
  exp.Embarked <- get_single_fisher(data$Embarked, data$Survived)

  expect_true(tibble::is_tibble(score_res))

  expect_identical(nrow(score_res), ncol(data) - 1L)

  expect_named(score_res, c("name", "score", "outcome", "predictor"))

  expect_identical(
    score_res$score,
    c(exp.Pclass, exp.Sex, exp.Age, exp.Fare, exp.Embarked)
  )

  expect_equal(unique(score_res$name), "pval_fisher")

  expect_equal(unique(score_res$outcome), "Survived")
})

# TODO Test fdr
# TODO Test multiclass
# TODO Test more after we add validators
