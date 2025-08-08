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

test_that("computations chisq test - adding adjusted p-values", {
  skip_if_not_installed("titanic")
  titanic_subset <- helper_titanic()

  titanic_xtab_pval_chisq_res <- score_xtab_pval_chisq |>
    fit(Survived ~ ., data = titanic_subset)

  natrual_units <- score_xtab_pval_chisq |> dont_log_pvalues()

  titanic_xtab_pval_natrual_res <-
    natrual_units |>
    fit(Survived ~ ., data = titanic_subset)

  # ----------------------------------------------------------------------------

  predictors <- titanic_xtab_pval_chisq_res@results$predictor
  for (predictor in predictors) {
    chisq <- titanic_xtab_pval_chisq_res@results[
      titanic_xtab_pval_chisq_res@results$predictor == predictor,
    ]
    nat <- titanic_xtab_pval_natrual_res@results[
      titanic_xtab_pval_natrual_res@results$predictor == predictor,
    ]
    nat_p_adj <- titanic_xtab_pval_natrual_res@results[
      titanic_xtab_pval_natrual_res@results$predictor == predictor,
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
        suppressWarnings(stats::chisq.test(tmp_tab)$p.value),
        silent = TRUE
      )

      if (inherits(fit_chisq, "try-error")) {
        fit_chisq <- NA_real_
      }
    }

    expect_equal(chisq$score, -log10(fit_chisq))
    expect_equal(nat$score, fit_chisq)
  }

  # ----------------------------------------------------------------------------
  # adjusted p-values

  titanic_xtab_pval_chisq_p_adj_res <- score_xtab_pval_chisq |>
    fit(Survived ~ ., data = titanic_subset, adjustment = "BH")

  titanic_xtab_pval_natrual_p_adj_res <-
    natrual_units |>
    fit(Survived ~ ., data = titanic_subset, adjustment = "BH")

  expect_equal(
    titanic_xtab_pval_chisq_p_adj_res@results$score,
    -log10(
      10^(-titanic_xtab_pval_chisq_res@results$score) |>
        stats::p.adjust(method = "BH")
    )
  )

  expect_equal(
    titanic_xtab_pval_natrual_p_adj_res@results$score,
    titanic_xtab_pval_natrual_res@results$score |>
      stats::p.adjust(method = "BH")
  )
})

test_that("computations fisher", {
  skip_if_not_installed("titanic")
  titanic_subset <- helper_titanic()

  titanic_xtab_pval_fisher_res <- score_xtab_pval_fisher |>
    fit(Survived ~ ., data = titanic_subset)

  natrual_units <- score_xtab_pval_fisher |> dont_log_pvalues()

  titanic_xtab_pval_natrual_res <-
    natrual_units |>
    fit(Survived ~ ., data = titanic_subset)
  # ----------------------------------------------------------------------------

  predictors <- titanic_xtab_pval_fisher_res@results$predictor
  for (predictor in predictors) {
    fisher <- titanic_xtab_pval_fisher_res@results[
      titanic_xtab_pval_fisher_res@results$predictor == predictor,
    ]
    nat <- titanic_xtab_pval_natrual_res@results[
      titanic_xtab_pval_natrual_res@results$predictor == predictor,
    ]

    tmp_data <- tibble::tibble(
      x = titanic_subset[[predictor]],
      y = titanic_subset$Survived
    )

    if (is.numeric(tmp_data$x)) {
      fit_fisher <- NA_real_
    } else {
      tmp_tab <- table(tmp_data$x, tmp_data$y)

      fit_fisher <- try(
        suppressWarnings(stats::fisher.test(tmp_tab)$p.value),
        silent = TRUE
      )

      if (inherits(fit_fisher, "try-error")) {
        fit_fisher <- NA_real_
      }
    }

    expect_equal(fisher$score, -log10(fit_fisher))
    expect_equal(nat$score, fit_fisher)
  }
})

test_that("computations chisq test - multiclass outcome", {
  skip_if_not_installed("modeldata")

  hpc_subset <- helper_hpc_data()

  set.seed(42)
  hpc_xtab_pval_chisq_res <- score_xtab_pval_chisq |>
    fit(class ~ ., data = hpc_subset)

  natrual_units <- score_xtab_pval_chisq |> dont_log_pvalues()

  set.seed(42)
  hpc_xtab_pval_natrual_res <-
    natrual_units |>
    fit(class ~ ., data = hpc_subset)

  # ----------------------------------------------------------------------------

  predictors <- hpc_xtab_pval_chisq_res@results$predictor
  for (predictor in predictors) {
    chisq <- hpc_xtab_pval_chisq_res@results[
      hpc_xtab_pval_chisq_res@results$predictor == predictor,
    ]
    nat <- hpc_xtab_pval_natrual_res@results[
      hpc_xtab_pval_natrual_res@results$predictor == predictor,
    ]

    tmp_data <- tibble::tibble(
      x = hpc_subset[[predictor]],
      y = hpc_subset$class
    )

    if (is.numeric(tmp_data$x)) {
      fit_chisq <- NA_real_
    } else {
      set.seed(42)
      tmp_tab <- table(tmp_data$x, sample(tmp_data$y))

      fit_chisq <- try(
        suppressWarnings(stats::chisq.test(tmp_tab)$p.value),
        silent = TRUE
      )

      if (inherits(fit_chisq, "try-error")) {
        fit_chisq <- NA_real_
      }
    }

    expect_equal(chisq$score, -log10(fit_chisq))
    expect_equal(nat$score, fit_chisq)
  }
})

# TODO Test more after we add validators
