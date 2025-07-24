test_that("cor object creation", {
  expect_s3_class(
    score_cor_pearson,
    c("filtro::class_score_cor", "filtro::class_score", "S7_object")
  )

  expect_s3_class(
    score_cor_spearman,
    c("filtro::class_score_cor", "filtro::class_score", "S7_object")
  )
})

test_that("cor computations", {
  perm_pearson_res <-
    score_cor_pearson |>
    fit(mpg ~ ., data = mtcars)

  perm_spearman_res <-
    score_cor_spearman |>
    fit(mpg ~ ., data = mtcars)

  # ----------------------------------------------------------------------------

  predictors <- perm_pearson_res@results$predictor
  for (predictor in predictors) {
    pearson <- perm_pearson_res@results[
      perm_pearson_res@results$predictor == predictor,
    ]
    spearman <- perm_spearman_res@results[
      perm_spearman_res@results$predictor == predictor,
    ]

    tmp_data <- tibble::tibble(x = mtcars[[predictor]], y = mtcars$mpg)
    pearson_exp <- stats::cor(tmp_data$x, tmp_data$y, method = "pearson")
    spearman_exp <- stats::cor(tmp_data$x, tmp_data$y, method = "spearman")

    expect_equal(pearson$score, pearson_exp)
    expect_equal(spearman$score, spearman_exp)
  }

  # ----------------------------------------------------------------------------

  expect_equal(perm_pearson_res@range, c(-1.0, 1.0))
  expect_equal(perm_pearson_res@inclusive, rep(TRUE, 2))
  expect_equal(perm_pearson_res@fallback_value, 1)
  expect_equal(perm_pearson_res@direction, "maximize")
})

test_that("aov computations - wrong variable types", {
  skip_if_not_installed("modeldata")

  perm_data <- helper_perm()
  perm_data <- perm_data |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("chem"), as.factor))

  perm_pearson_res <-
    score_cor_pearson |>
    fit(permeability ~ ., data = perm_data)

  expect_true(all(is.na(perm_pearson_res@results$score)))
})

test_that("aov computations - required packages", {
  expect_equal(required_pkgs(score_cor_pearson), "filtro")
  expect_equal(required_pkgs(score_cor_spearman), "filtro")
})
