test_that("good results - based on prop", {
  skip_if_not_installed("desirability2")
  library(desirability2) # Otherwise Error in `compute_desirability_scores(res, mtr = mtr)`

  ames_scores_results <- helper_ames_res()

  des_1 <- ames_scores_results |>
    show_best_desirability_prop(
      maximize(cor_pearson, low = 0, high = 1),
      maximize(imp_rf),
      maximize(infogain)
    )

  # ----------------------------------------------------------------------------

  mtr <- des_1
  d_cor_pearson_1 <- desirability2::d_max(mtr$cor_pearson, low = 0, high = 1)
  d_imp_rf_1 <- desirability2::d_max(
    mtr$imp_rf,
    low = min(mtr$imp_rf),
    high = max(mtr$imp_rf)
  )
  d_infogain_1 <- desirability2::d_max(
    mtr$infogain,
    low = min(mtr$infogain),
    high = max(mtr$infogain)
  )

  expect_equal(des_1$.d_max_cor_pearson, d_cor_pearson_1)
  expect_equal(des_1$.d_max_imp_rf, d_imp_rf_1)
  expect_equal(des_1$.d_max_infogain, d_infogain_1)

  expect_named(
    des_1,
    c(
      "predictor",
      "aov_pval",
      "cor_pearson",
      "imp_rf",
      "infogain",
      ".d_max_cor_pearson",
      ".d_max_imp_rf",
      ".d_max_infogain",
      ".d_overall"
    )
  )
})

test_that("good results - based on num", {
  skip_if_not_installed("desirability2")
  library(desirability2) # Otherwise Error in `compute_desirability_scores(res, mtr = mtr)`

  ames_scores_results <- helper_ames_res()

  des_1 <- ames_scores_results |>
    show_best_desirability_num(
      maximize(cor_pearson, low = 0, high = 1),
      maximize(imp_rf),
      maximize(infogain)
    )

  # ----------------------------------------------------------------------------

  mtr <- des_1
  d_cor_pearson_1 <- desirability2::d_max(mtr$cor_pearson, low = 0, high = 1)
  d_imp_rf_1 <- desirability2::d_max(
    mtr$imp_rf,
    low = min(mtr$imp_rf),
    high = max(mtr$imp_rf)
  )
  d_infogain_1 <- desirability2::d_max(
    mtr$infogain,
    low = min(mtr$infogain),
    high = max(mtr$infogain)
  )

  expect_equal(des_1$.d_max_cor_pearson, d_cor_pearson_1)
  expect_equal(des_1$.d_max_imp_rf, d_imp_rf_1)
  expect_equal(des_1$.d_max_infogain, d_infogain_1)

  expect_named(
    des_1,
    c(
      "predictor",
      "aov_pval",
      "cor_pearson",
      "imp_rf",
      "infogain",
      ".d_max_cor_pearson",
      ".d_max_imp_rf",
      ".d_max_infogain",
      ".d_overall"
    )
  )
})

# TODO Test missing data
# TODO Test bad results
