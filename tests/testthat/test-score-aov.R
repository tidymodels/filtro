test_that("aov object creation", {
  expect_equal(
    class(score_aov_fstat),
    c("filtro::class_score_aov", "filtro::class_score", "S7_object")
  )

  expect_equal(
    class(score_aov_pval),
    c("filtro::class_score_aov", "filtro::class_score", "S7_object")
  )
})

test_that("aov computations - class outcome", {
  skip_if_not_installed("modeldata")
  cell_data <- helper_cells()

  cell_fstat_res <-
    score_aov_fstat |>
    fit(class ~ ., data = cell_data)

  cell_pval_res <-
    score_aov_pval |>
    fit(class ~ ., data = cell_data)

  natrual_units <- score_aov_pval |> dont_log_pvalues()
  #natrual_units@neg_log10 <- FALSE

  cell_pval_natrual_res <-
    natrual_units |>
    fit(class ~ ., data = cell_data)

  # ----------------------------------------------------------------------------

  predictors <- cell_fstat_res@results$predictor
  for (i in predictors) {
    tmp_data <- tibble::tibble(y = cell_data[[i]], x = cell_data$class)
    fit <- lm(y ~ x, data = tmp_data)
    fit_aov <- anova(fit)

    fstat <- cell_fstat_res@results[cell_fstat_res@results$predictor == i, ]
    lg10 <- cell_pval_res@results[cell_fstat_res@results$predictor == i, ]
    nat <- cell_pval_natrual_res@results[
      cell_fstat_res@results$predictor == i,
    ]

    expect_equal(fstat$score, fit_aov[1, "F value"])
    expect_equal(lg10$score, -log10(fit_aov[1, "Pr(>F)"]))
    expect_equal(nat$score, fit_aov[1, "Pr(>F)"])
  }

  # ----------------------------------------------------------------------------

  expect_equal(cell_pval_natrual_res@range, c(0.0, 1.0))
  expect_equal(cell_pval_natrual_res@inclusive, rep(TRUE, 2))
  expect_equal(cell_pval_natrual_res@fallback_value, .Machine$double.neg.eps)
  expect_equal(
    cell_pval_natrual_res@sorts,
    function(x) x,
    ignore_function_env = TRUE
  )
  expect_equal(cell_pval_natrual_res@direction, "minimize")
})


test_that("aov computations - numeric outcome", {
  skip_if_not_installed("modeldata")
  perm_data <- helper_perm_factors()

  perm_fstat_res <-
    score_aov_fstat |>
    fit(permeability ~ ., data = perm_data)

  perm_pval_res <-
    score_aov_pval |>
    fit(permeability ~ ., data = perm_data)

  natrual_units <- score_aov_pval
  natrual_units@neg_log10 <- FALSE

  perm_pval_natrual_res <-
    natrual_units |>
    fit(permeability ~ ., data = perm_data)

  # ----------------------------------------------------------------------------

  predictors <- perm_fstat_res@results$predictor
  for (i in predictors) {
    fstat <- perm_fstat_res@results[perm_fstat_res@results$predictor == i, ]
    lg10 <- perm_pval_res@results[perm_fstat_res@results$predictor == i, ]
    nat <- perm_pval_natrual_res@results[
      perm_fstat_res@results$predictor == i,
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
  expect_equal(
    perm_pval_natrual_res@sorts,
    function(x) x,
    ignore_function_env = TRUE
  )
  expect_equal(perm_pval_natrual_res@direction, "minimize")
})

test_that("aov computations - wrong variable types", {
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

test_that("aov computations - required packages", {
  expect_equal(required_pkgs(score_aov_pval), "filtro")
})
