test_that("object creation", {
  expect_s3_class(
    score_xtab_pval_chisq,
    c("filtro::class_score_xtab", "filtro::class_score", "S7_object")
  )

  expect_s3_class(
    score_xtab_pval_fisher,
    c("filtro::class_score_xtab", "filtro::class_score", "S7_object")
  )
})

test_that("computations", {
  skip_if_not_installed("titanic")
  titanic_subset <- helper_titanic()
  titanic_xtab_pval_chisq_res <- score_xtab_pval_chisq |>
    fit(Survived ~ ., data = titanic_subset)

  # ----------------------------------------------------------------------------

  predictors <- titanic_xtab_pval_chisq_res@results$predictor
  for (predictor in predictors) {
    chisq <- titanic_xtab_pval_chisq_res@results[
      titanic_xtab_pval_chisq_res@results$predictor == predictor,
    ]

    tmp_data <- tibble::tibble(
      x = titanic_subset[[predictor]],
      y = titanic_subset$Survived
    )

    if (is.numeric(tmp_data$x)) {
      fit_chisq <- NA_real_
    } else {
      tmp_tab <- table(tmp_data$x, tmp_data$y)

      fit_chisq <- try(
        suppressWarnings(-log10(stats::chisq.test(tmp_tab)$p.value)),
        silent = TRUE
      )

      if (inherits(fit_chisq, "try-error")) {
        fit_chisq <- NA_real_
      }
    }

    expect_equal(chisq$score, fit_chisq)
  }
})

skip()

titanic_xtab_pval_fisher_res <- score_xtab_pval_fisher |>
  fit(Survived ~ ., data = titanic_subset)
titanic_xtab_pval_fisher_res@results

test_that("get_scores_cross_tab is working for -log10(chisq pval)", {
  if (rlang::is_installed("titanic")) {
    library(titanic)

    titanic_train <- titanic_train |>
      dplyr::mutate(dplyr::across(
        c(Survived, Pclass, Sex, Embarked),
        as.factor
      ))
    titanic_subset <- titanic_train |>
      dplyr::select(Survived, Pclass, Sex, Age, Fare, Embarked)

    score_obj <- score_cross_tab(score_type = "pval_chisq")
    score_res <- get_scores_cross_tab(
      score_obj,
      data = titanic_subset,
      outcome = "Survived"
    )

    exp.Pclass <- get_single_chisq(
      titanic_subset$Pclass,
      titanic_subset$Survived
    )
    exp.Pclass <- -log10(exp.Pclass)
    exp.Sex <- get_single_chisq(titanic_subset$Sex, titanic_subset$Survived)
    exp.Sex <- -log10(exp.Sex)
    exp.Age <- NA
    exp.Fare <- NA
    exp.Embarked <- get_single_chisq(
      titanic_subset$Embarked,
      titanic_subset$Survived
    )
    exp.Embarked <- -log10(exp.Embarked)

    expect_true(tibble::is_tibble(score_res))

    expect_identical(nrow(score_res), ncol(titanic_subset) - 1L)

    expect_named(score_res, c("name", "score", "outcome", "predictor"))

    expect_identical(
      score_res$score,
      c(exp.Pclass, exp.Sex, exp.Age, exp.Fare, exp.Embarked)
    )

    expect_equal(unique(score_res$name), "pval_chisq")

    expect_equal(unique(score_res$outcome), "Survived")
  }
})

test_that("get_scores_cross_tab is working for chisq pval", {
  if (rlang::is_installed("titanic")) {
    library(titanic)

    titanic_train <- titanic_train |>
      dplyr::mutate(dplyr::across(
        c(Survived, Pclass, Sex, Embarked),
        as.factor
      ))
    titanic_subset <- titanic_train |>
      dplyr::select(Survived, Pclass, Sex, Age, Fare, Embarked)

    score_obj <- score_cross_tab(
      score_type = "pval_chisq",
      neg_log10 = FALSE,
      direction = "minimize",
      fallback_value = 0
    )
    score_res <- get_scores_cross_tab(
      score_obj,
      data = titanic_subset,
      outcome = "Survived"
    )

    exp.Pclass <- get_single_chisq(
      titanic_subset$Pclass,
      titanic_subset$Survived
    )
    exp.Sex <- get_single_chisq(titanic_subset$Sex, titanic_subset$Survived)
    exp.Age <- NA
    exp.Fare <- NA
    exp.Embarked <- get_single_chisq(
      titanic_subset$Embarked,
      titanic_subset$Survived
    )

    expect_true(tibble::is_tibble(score_res))

    expect_identical(nrow(score_res), ncol(titanic_subset) - 1L)

    expect_named(score_res, c("name", "score", "outcome", "predictor"))

    expect_identical(
      score_res$score,
      c(exp.Pclass, exp.Sex, exp.Age, exp.Fare, exp.Embarked)
    )

    expect_equal(unique(score_res$name), "pval_chisq")

    expect_equal(unique(score_res$outcome), "Survived")
  }
})

test_that("get_score_cross_tab is working for -log10(fisher pval)", {
  if (rlang::is_installed("titanic")) {
    library(titanic)

    titanic_train <- titanic_train |>
      dplyr::mutate(dplyr::across(
        c(Survived, Pclass, Sex, Embarked),
        as.factor
      ))
    titanic_subset <- titanic_train |>
      dplyr::select(Survived, Pclass, Sex, Age, Fare, Embarked)

    score_obj <- score_cross_tab(score_type = "pval_fisher")
    score_res <- get_scores_cross_tab(
      score_obj,
      data = titanic_subset,
      outcome = "Survived"
    )

    exp.Pclass <- get_single_fisher(
      titanic_subset$Pclass,
      titanic_subset$Survived
    )
    exp.Pclass <- -log10(exp.Pclass)
    exp.Sex <- get_single_fisher(titanic_subset$Sex, titanic_subset$Survived)
    exp.Sex <- -log10(exp.Sex)
    exp.Age <- NA
    exp.Fare <- NA
    exp.Embarked <- get_single_fisher(
      titanic_subset$Embarked,
      titanic_subset$Survived
    )
    exp.Embarked <- -log10(exp.Embarked)

    expect_true(tibble::is_tibble(score_res))

    expect_identical(nrow(score_res), ncol(titanic_subset) - 1L)

    expect_named(score_res, c("name", "score", "outcome", "predictor"))

    expect_identical(
      score_res$score,
      c(exp.Pclass, exp.Sex, exp.Age, exp.Fare, exp.Embarked)
    )

    expect_equal(unique(score_res$name), "pval_fisher")

    expect_equal(unique(score_res$outcome), "Survived")
  }
})

test_that("get_score_cross_tab is working for fisher pval", {
  if (rlang::is_installed("titanic")) {
    library(titanic)

    titanic_train <- titanic_train |>
      dplyr::mutate(dplyr::across(
        c(Survived, Pclass, Sex, Embarked),
        as.factor
      ))
    titanic_subset <- titanic_train |>
      dplyr::select(Survived, Pclass, Sex, Age, Fare, Embarked)

    score_obj <- score_cross_tab(
      score_type = "pval_fisher",
      neg_log10 = FALSE,
      direction = "minimize",
      fallback_value = 0
    )
    score_res <- get_scores_cross_tab(
      score_obj,
      data = titanic_subset,
      outcome = "Survived"
    )

    exp.Pclass <- get_single_fisher(
      titanic_subset$Pclass,
      titanic_subset$Survived
    )
    exp.Sex <- get_single_fisher(titanic_subset$Sex, titanic_subset$Survived)
    exp.Age <- NA
    exp.Fare <- NA
    exp.Embarked <- get_single_fisher(
      titanic_subset$Embarked,
      titanic_subset$Survived
    )

    expect_true(tibble::is_tibble(score_res))

    expect_identical(nrow(score_res), ncol(titanic_subset) - 1L)

    expect_named(score_res, c("name", "score", "outcome", "predictor"))

    expect_identical(
      score_res$score,
      c(exp.Pclass, exp.Sex, exp.Age, exp.Fare, exp.Embarked)
    )

    expect_equal(unique(score_res$name), "pval_fisher")

    expect_equal(unique(score_res$outcome), "Survived")
  }
})

# TODO Test fdr
# TODO Test multiclass
# TODO Test more after we add validators

library(tidymodels)
hpc_subset <- helper_hpc_data()
