test_that("object creation", {
  expect_s3_class(
    score_aov_fstat,
    c("filtro::class_score_aov", "filtro::class_score", "S7_object")
  )

  expect_s3_class(
    score_aov_pval,
    c("filtro::class_score_aov", "filtro::class_score", "S7_object")
  )
})

test_that("computations - class outcome", {
  skip_if_not_installed("modeldata")

  cell_data <- helper_cells()

  cell_fstat_res <-
    score_aov_fstat |>
    fit(class ~ ., data = cell_data)

  cell_pval_res <-
    score_aov_pval |>
    fit(class ~ ., data = cell_data)

  natrual_units <- score_aov_pval |> dont_log_pvalues()

  cell_pval_natrual_res <-
    natrual_units |>
    fit(class ~ ., data = cell_data)

  # ----------------------------------------------------------------------------

  predictors <- cell_fstat_res@results$predictor
  for (predictor in predictors) {
    tmp_data <- tibble::tibble(y = cell_data[[predictor]], x = cell_data$class)
    fit <- lm(y ~ x, data = tmp_data)
    fit_aov <- anova(fit)

    fstat <- cell_fstat_res@results[
      cell_fstat_res@results$predictor == predictor,
    ]
    lg10 <- cell_pval_res@results[
      cell_pval_res@results$predictor == predictor,
    ]
    nat <- cell_pval_natrual_res@results[
      cell_pval_natrual_res@results$predictor == predictor,
    ]

    expect_equal(fstat$score, fit_aov[1, "F value"])
    expect_equal(lg10$score, -log10(fit_aov[1, "Pr(>F)"]))
    expect_equal(nat$score, fit_aov[1, "Pr(>F)"])
  }

  # ----------------------------------------------------------------------------

  expect_equal(cell_pval_natrual_res@range, c(0.0, 1.0))
  expect_equal(cell_pval_natrual_res@inclusive, rep(TRUE, 2))
  expect_equal(cell_pval_natrual_res@fallback_value, .Machine$double.neg.eps)
  expect_equal(cell_pval_natrual_res@direction, "minimize")
})

test_that("computations - numeric outcome", {
  skip_if_not_installed("modeldata")

  perm_data <- helper_perm_factors()

  perm_fstat_res <-
    score_aov_fstat |>
    fit(permeability ~ ., data = perm_data)

  perm_pval_res <-
    score_aov_pval |>
    fit(permeability ~ ., data = perm_data)

  natrual_units <- score_aov_pval |> dont_log_pvalues()

  perm_pval_natrual_res <-
    natrual_units |>
    fit(permeability ~ ., data = perm_data)

  # ----------------------------------------------------------------------------

  predictors <- perm_fstat_res@results$predictor
  for (i in predictors) {
    fstat <- perm_fstat_res@results[perm_fstat_res@results$predictor == i, ]
    lg10 <- perm_pval_res@results[perm_pval_res@results$predictor == i, ]
    nat <- perm_pval_natrual_res@results[
      perm_pval_natrual_res@results$predictor == i,
    ]

    num_x <- length(unique(perm_data[[i]]))
    if (num_x == 1) {
      expect_equal(fstat$score, NA_real_)
      expect_equal(lg10$score, NA_real_)
      expect_equal(nat$score, NA_real_)
    } else {
      tmp_data <- tibble::tibble(x = perm_data[[i]], y = perm_data$permeability)
      fit <- lm(y ~ x, data = tmp_data)
      fit_aov <- anova(fit)

      expect_equal(fstat$score, fit_aov[1, "F value"])
      expect_equal(lg10$score, -log10(fit_aov[1, "Pr(>F)"]))
      expect_equal(nat$score, fit_aov[1, "Pr(>F)"])
    }
  }

  # ----------------------------------------------------------------------------

  expect_equal(perm_pval_natrual_res@range, c(0.0, 1.0))
  expect_equal(perm_pval_natrual_res@inclusive, rep(TRUE, 2))
  expect_equal(perm_pval_natrual_res@fallback_value, .Machine$double.neg.eps)
  expect_equal(perm_pval_natrual_res@direction, "minimize")
})

test_that("computations - wrong variable types", {
  skip_if_not_installed("modeldata")

  perm_data <- helper_perm()

  perm_fstat_res <-
    score_aov_fstat |>
    fit(permeability ~ ., data = perm_data)

  expect_true(all(is.na(perm_fstat_res@results$score)))

  ###

  ames_data <- helper_ames_v2() |>
    dplyr::select(-Lot_Frontage, -Lot_Area, -Sale_Price)

  ames_fstat_res <-
    score_aov_fstat |>
    fit(Utilities ~ ., data = ames_data)

  expect_true(all(is.na(ames_fstat_res@results$score)))

  ###

  ames_data_chr <-
    helper_ames_v2() |>
    dplyr::select(-Lot_Frontage, -Lot_Area) |>
    dplyr::mutate(dplyr::across(c(-Sale_Price), as.character))

  ames_chr_res <-
    score_aov_fstat |>
    fit(Sale_Price ~ ., data = ames_data_chr)

  expect_true(all(is.na(ames_chr_res@results$score)))
})

test_that("computations - required packages", {
  expect_equal(required_pkgs(score_aov_fstat), "filtro")
  expect_equal(required_pkgs(score_aov_pval), "filtro")
})

test_that("aov filters - adding missing values and case weights", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()

  ames_fstat_res <-
    score_aov_fstat |>
    fit(Sale_Price ~ ., data = ames_subset)

  ames_pval_res <-
    score_aov_pval |>
    fit(Sale_Price ~ ., data = ames_subset)

  natrual_units <- score_aov_pval
  natrual_units@neg_log10 <- FALSE

  ames_pval_natrual_res <-
    natrual_units |>
    fit(Sale_Price ~ ., data = ames_subset)

  # ----------------------------------------------------------------------------

  predictors <- ames_fstat_res@results$predictor
  for (predictor in predictors) {
    fstat <- ames_fstat_res@results[
      ames_fstat_res@results$predictor == predictor,
    ]
    lg10 <- ames_pval_res@results[
      ames_pval_res@results$predictor == predictor,
    ]
    nat <- ames_pval_natrual_res@results[
      ames_pval_natrual_res@results$predictor == predictor,
    ]

    tmp_data <- tibble::tibble(
      x = ames_subset[[predictor]],
      y = ames_subset$Sale_Price
    )

    if (is.numeric(tmp_data$x)) {
      fit_aov <- NA_real_
    } else {
      fit <- try(lm(y ~ x, data = tmp_data), silent = TRUE)

      if (inherits(fit, "try-error")) {
        fit_aov <- NA_real_
      } else {
        fit_aov <- try(anova(fit), silent = TRUE)
        if (inherits(fit_aov, "try-error")) {
          fit_aov <- NA_real_
        }
      }

      expect_equal(fstat$score, fit_aov[1, "F value"])
      expect_equal(lg10$score, -log10(fit_aov[1, "Pr(>F)"]))
      expect_equal(nat$score, fit_aov[1, "Pr(>F)"])
    }
  }

  # ----------------------------------------------------------------------------
  # missing values

  ames_missing <- ames_subset
  ames_missing$Sale_Price[1] <- NA_real_
  ames_missing$Lot_Frontage[2] <- NA_real_

  ames_missing_fstat_res <-
    score_aov_fstat |>
    fit(Sale_Price ~ ., data = ames_missing)

  exp.Lot_Frontage <- exp.Lot_Area <- NA

  exp.MS_SubClass <- anova(lm(
    Sale_Price ~ MS_SubClass,
    data = ames_missing,
    na.action = "na.omit"
  ))[1, "F value"]

  exp.MS_Zoning <- anova(lm(
    Sale_Price ~ MS_Zoning,
    data = ames_missing,
    na.action = "na.omit"
  ))[1, "F value"]

  exp.Street <- anova(lm(
    Sale_Price ~ Street,
    data = ames_missing,
    na.action = "na.omit"
  ))[1, "F value"]

  expect_identical(
    ames_missing_fstat_res@results$score,
    c(
      exp.MS_SubClass,
      exp.MS_Zoning,
      exp.Lot_Frontage,
      exp.Lot_Area,
      exp.Street
    ),
    tolerance = 0.01
  )

  # ----------------------------------------------------------------------------
  # case weights

  two_weights <- c(rep(1, 10), rep(0, nrow(ames_subset) - 10))

  ames_subset <- ames_subset |> dplyr::select(-Street)

  ames_weights_fstat_res <-
    score_aov_fstat |>
    fit(Sale_Price ~ ., data = ames_subset, case_weights = two_weights)

  exp.Lot_Frontage <- exp.Lot_Area <- NA

  exp.MS_SubClass <- anova(lm(
    Sale_Price ~ MS_SubClass,
    data = ames_subset,
    weights = two_weights,
    na.action = "na.omit"
  ))[1, "F value"]

  exp.MS_Zoning <- anova(lm(
    Sale_Price ~ MS_Zoning,
    data = ames_subset,
    weights = two_weights,
    na.action = "na.omit"
  ))[1, "F value"]

  expect_identical(
    ames_weights_fstat_res@results$score,
    c(
      exp.MS_SubClass,
      exp.MS_Zoning,
      exp.Lot_Frontage,
      exp.Lot_Area
    ),
    tolerance = 0.01
  )
})
