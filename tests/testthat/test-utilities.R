test_that("computations - arrange score", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()
  ames_subset <- ames_subset |>
    dplyr::mutate(Sale_Price = log10(Sale_Price))

  ames_aov_pval_res <-
    score_aov_pval |>
    fit(Sale_Price ~ ., data = ames_subset)

  ames_aov_fstat_res <-
    score_aov_fstat |>
    fit(Sale_Price ~ ., data = ames_subset)

  # ----------------------------------------------------------------------------

  pval_res <- ames_aov_pval_res |> arrange_score()
  exp_pval_res <- ames_aov_pval_res@results |>
    dplyr::arrange(dplyr::desc(score))

  fstat_res <- ames_aov_fstat_res |> arrange_score()
  exp_fstat_res <- ames_aov_fstat_res@results |>
    dplyr::arrange(dplyr::desc(score))

  expect_equal(pval_res$score, exp_pval_res$score)
  expect_equal(fstat_res$score, exp_fstat_res$score)
})

test_that("computations - fill safe value", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()
  ames_subset <- ames_subset |>
    dplyr::mutate(Sale_Price = log10(Sale_Price))

  ames_aov_pval_res <-
    score_aov_pval |>
    fit(Sale_Price ~ ., data = ames_subset)

  # ----------------------------------------------------------------------------

  pval_res <- ames_aov_pval_res |> fill_safe_value(return_results = TRUE)

  is_na_score <- is.na(ames_aov_pval_res@results$score)
  ames_aov_pval_res@results$score[
    is_na_score
  ] <- ames_aov_pval_res@fallback_value

  exp_pval_res <- ames_aov_pval_res@results

  expect_equal(pval_res$score, exp_pval_res$score)

  # ----------------------------------------------------------------------------
  # used with show best score based on prop_terms

  pval_res_fill_1 <- ames_aov_pval_res |>
    fill_safe_value() |>
    show_best_score_prop(prop_terms = 0.2)

  pval_res_fill_2 <- ames_aov_pval_res |>
    fill_safe_value() |>
    show_best_score_prop(prop_terms = 0.8)

  is_na_score <- is.na(ames_aov_pval_res@results$score)
  ames_aov_pval_res@results$score[
    is_na_score
  ] <- ames_aov_pval_res@fallback_value

  exp_pval_res_1 <- ames_aov_pval_res@results |>
    dplyr::slice_max(score, prop = 0.2)

  exp_pval_res_2 <- ames_aov_pval_res@results |>
    dplyr::slice_max(score, prop = 0.8)

  expect_equal(pval_res_fill_1$score, exp_pval_res_1$score)
  expect_equal(pval_res_fill_2$score, exp_pval_res_2$score)
})

test_that("computations - show best score based on prop_terms", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()
  ames_subset <- ames_subset |>
    dplyr::mutate(Sale_Price = log10(Sale_Price))

  ames_aov_pval_res <-
    score_aov_pval |>
    fit(Sale_Price ~ ., data = ames_subset)

  # ----------------------------------------------------------------------------

  pval_res_1 <- ames_aov_pval_res |> show_best_score_prop(prop_terms = 0.2)
  pval_res_2 <- ames_aov_pval_res |> show_best_score_prop(prop_terms = 0.4)
  pval_res_3 <- ames_aov_pval_res |> show_best_score_prop(prop_terms = 0.6)
  pval_res_4 <- ames_aov_pval_res |> show_best_score_prop(prop_terms = 0.8)

  exp_pval_res_1 <- ames_aov_pval_res@results |>
    dplyr::slice_max(score, prop = 0.2)
  exp_pval_res_2 <- ames_aov_pval_res@results |>
    dplyr::slice_max(score, prop = 0.4)
  exp_pval_res_3 <- ames_aov_pval_res@results |>
    dplyr::slice_max(score, prop = 0.6)
  exp_pval_res_4 <- ames_aov_pval_res@results |>
    dplyr::slice_max(score, prop = 0.8)

  expect_equal(pval_res_1$score, exp_pval_res_1$score)
  expect_equal(pval_res_2$score, exp_pval_res_2$score)
  expect_equal(pval_res_3$score, exp_pval_res_3$score)
  expect_equal(pval_res_4$score, exp_pval_res_4$score)
})

test_that("object creation - S7 subclass of base R's list", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()
  ames_subset <- ames_subset |>
    dplyr::mutate(Sale_Price = log10(Sale_Price))

  # anova pval
  ames_aov_pval_res <-
    score_aov_pval |>
    fit(Sale_Price ~ ., data = ames_subset)
  ames_aov_pval_res@results

  # cor
  ames_cor_pearson_res <-
    score_cor_pearson |>
    fit(Sale_Price ~ ., data = ames_subset)
  ames_cor_pearson_res@results

  # forest imp
  set.seed(42)
  ames_imp_rf_reg_res <-
    score_imp_rf |>
    fit(Sale_Price ~ ., data = ames_subset)
  ames_imp_rf_reg_res@results

  # info gain
  ames_info_gain_reg_res <-
    score_info_gain |>
    fit(Sale_Price ~ ., data = ames_subset)
  ames_info_gain_reg_res@results

  # Create a list
  class_score_list <- list(
    ames_aov_pval_res,
    ames_cor_pearson_res,
    ames_imp_rf_reg_res,
    ames_info_gain_reg_res
  )

  expect_equal(class(class_score_list), "list")
})

skip()

# Bind score class object, including their associated metadata and scores
ames_subset <- helper_ames()
ames_subset <- ames_subset |>
  dplyr::mutate(Sale_Price = log10(Sale_Price))

# anova pval
ames_aov_pval_res <-
  score_aov_pval |>
  fit(Sale_Price ~ ., data = ames_subset)
ames_aov_pval_res@results

# cor
ames_cor_pearson_res <-
  score_cor_pearson |>
  fit(Sale_Price ~ ., data = ames_subset)
ames_cor_pearson_res@results

# forest imp
set.seed(42)
ames_imp_rf_reg_res <-
  score_imp_rf |>
  fit(Sale_Price ~ ., data = ames_subset)
ames_imp_rf_reg_res@results

# info gain
ames_info_gain_reg_res <-
  score_info_gain |>
  fit(Sale_Price ~ ., data = ames_subset)
ames_info_gain_reg_res@results

# Create a list
class_score_list <- list(
  ames_aov_pval_res,
  ames_cor_pearson_res,
  ames_imp_rf_reg_res,
  ames_info_gain_reg_res
)

# Bind scores
class_score_list |> bind_scores()

# Fill safe values
class_score_list |> fill_safe_values()

# Show best score based on number of predictors

ames_subset <- helper_ames_full()
ames_subset <- ames_subset |>
  dplyr::mutate(Sale_Price = log10(Sale_Price))

ames_aov_pval_res <-
  score_aov_pval |>
  fit(Sale_Price ~ ., data = ames_subset)

ames_aov_fstat_res <-
  score_aov_fstat |>
  fit(Sale_Price ~ ., data = ames_subset)

ames_aov_pval_res@results
ames_aov_pval_res |> show_best_score_num(num_terms = 2)

ames_aov_fstat_res@results
ames_aov_fstat_res |> show_best_score_num(num_terms = 2)

# Show best score based on cutoff value

ames_subset <- helper_ames()
ames_subset <- ames_subset |>
  dplyr::mutate(Sale_Price = log10(Sale_Price))

ames_aov_pval_res <-
  score_aov_pval |>
  fit(Sale_Price ~ ., data = ames_subset)

ames_aov_fstat_res <-
  score_aov_fstat |>
  fit(Sale_Price ~ ., data = ames_subset)

ames_aov_pval_res@results
ames_aov_pval_res |> show_best_score_cutoff(cutoff = 130)

ames_aov_fstat_res@results
ames_aov_fstat_res |> show_best_score_cutoff(cutoff = 94.5)

# Show best score based on proportion of predictors with
# optional cutoff value

ames_subset <- helper_ames()
ames_subset <- ames_subset |>
  dplyr::mutate(Sale_Price = log10(Sale_Price))

ames_aov_pval_res <-
  score_aov_pval |>
  fit(Sale_Price ~ ., data = ames_subset)

ames_aov_fstat_res <-
  score_aov_fstat |>
  fit(Sale_Price ~ ., data = ames_subset)

ames_aov_pval_res@results
ames_aov_pval_res |> show_best_score_dual(prop_terms = 0.5)
ames_aov_pval_res |> show_best_score_dual(prop_terms = 0.5, cutoff = 130)

ames_aov_pval_res@results
ames_aov_pval_res |> show_best_score_dual(num_terms = 2)
ames_aov_pval_res |> show_best_score_dual(prop_terms = 2, cutoff = 130)

# Rank score based on min_rank()
ames_subset <- helper_ames()
ames_subset <- ames_subset |>
  dplyr::mutate(Sale_Price = log10(Sale_Price))

ames_aov_pval_res <-
  score_aov_pval |>
  fit(Sale_Price ~ ., data = ames_subset)

ames_aov_fstat_res <-
  score_aov_fstat |>
  fit(Sale_Price ~ ., data = ames_subset)

ames_aov_pval_res@results
ames_aov_pval_res |> filtro::rank_best_score_min()

ames_aov_fstat_res@results
ames_aov_fstat_res |> filtro::rank_best_score_min()

# Rank score based on dense_rank()
ames_subset <- helper_ames()
ames_subset <- ames_subset |>
  dplyr::mutate(Sale_Price = log10(Sale_Price))

ames_aov_pval_res <-
  score_aov_pval |>
  fit(Sale_Price ~ ., data = ames_subset)

ames_aov_fstat_res <-
  score_aov_fstat |>
  fit(Sale_Price ~ ., data = ames_subset)

ames_aov_pval_res@results
ames_aov_pval_res |> filtro::rank_best_score_dense()

ames_aov_fstat_res@results
ames_aov_fstat_res |> filtro::rank_best_score_dense()
