test_that("good results", {
  skip_if_not_installed("desirability2")
  library(desirability2) # Otherwise Error in `compute_desirability_scores(res, mtr = mtr)`

  ames_scores_results <- helper_ames_res()

  des_1 <- ames_scores_results |>
    show_best_desirability_prop(
      maximize(cor_pearson, low = 0, high = 1)
    )

  mtr <- des_1

  d_cor_pearson_1 <- desirability2::d_max(mtr$cor_pearson, low = 0, high = 1)
  expect_equal(des_1$.d_max_cor_pearson, d_cor_pearson_1)

  expect_named(
    des_1,
    c(
      "predictor",
      "aov_pval",
      "cor_pearson",
      "imp_rf",
      "infogain",
      ".d_max_cor_pearson",
      ".d_overall"
    )
  )
})

skip()

library(filtro)
utils::data("ames_scores_results", package = "filtro")
ames_scores_results <- ames_scores_results |>
  dplyr::select(-outcome)
ames_scores_results

# show_best_desirability_prop
library(desirability2)

ames_scores_results |>
  show_best_desirability_prop(
    maximize(cor_pearson, low = 0, high = 1)
  )

ames_scores_results |>
  show_best_desirability_prop(
    maximize(cor_pearson, low = 0, high = 1),
    maximize(imp_rf)
  )

ames_scores_results |>
  show_best_desirability_prop(
    maximize(cor_pearson, low = 0, high = 1),
    maximize(imp_rf),
    maximize(infogain)
  )

ames_scores_results |>
  show_best_desirability_prop(
    maximize(cor_pearson, low = 0, high = 1),
    maximize(imp_rf),
    maximize(infogain),
    prop_terms = 0.2
  )

ames_scores_results |>
  show_best_desirability_prop(
    target(cor_pearson, low = 0.2, target = 0.255, high = 0.9)
  )

ames_scores_results |>
  show_best_desirability_prop(
    constrain(cor_pearson, low = 0.2, high = 1)
  )

# show_best_desirability_num

ames_scores_results |>
  show_best_desirability_num(
    maximize(cor_pearson, low = 0, high = 1)
  )

ames_scores_results |>
  show_best_desirability_num(
    maximize(cor_pearson, low = 0, high = 1),
    maximize(imp_rf)
  )

ames_scores_results |>
  show_best_desirability_num(
    maximize(cor_pearson, low = 0, high = 1),
    maximize(imp_rf),
    maximize(infogain)
  )

ames_scores_results |>
  show_best_desirability_num(
    maximize(cor_pearson, low = 0, high = 1),
    maximize(imp_rf),
    maximize(infogain),
    num_terms = 2
  )
