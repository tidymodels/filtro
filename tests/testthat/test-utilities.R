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

  pval_res <- ames_aov_pval_res |> arrange_score()

  fstat_res <- ames_aov_fstat_res |> arrange_score()

  # ----------------------------------------------------------------------------

  exp_pval_res <- ames_aov_pval_res@results |>
    dplyr::arrange(dplyr::desc(score))

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

  pval_res <- ames_aov_pval_res |> fill_safe_value(return_results = TRUE)

  # ----------------------------------------------------------------------------

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

test_that("computations - fill safe value adding transformation", {
  mtcars_cor_pearson_res <-
    score_cor_pearson |>
    fit(mpg ~ ., data = mtcars)

  res <- mtcars_cor_pearson_res |> fill_safe_value(return_results = TRUE)

  res_transform <- mtcars_cor_pearson_res |>
    fill_safe_value(return_results = TRUE, transform = TRUE)

  # ----------------------------------------------------------------------------

  is_na_score <- is.na(mtcars_cor_pearson_res@results$score)
  mtcars_cor_pearson_res@results$score[
    is_na_score
  ] <- mtcars_cor_pearson_res@fallback_value

  exp_res <- mtcars_cor_pearson_res@results

  expect_equal(res$score, exp_res$score)
  expect_equal(res_transform$score, abs(exp_res$score))

  # ----------------------------------------------------------------------------
  # used with show best score based on prop_terms

  res_fill_1 <- mtcars_cor_pearson_res |>
    fill_safe_value() |>
    show_best_score_prop(prop_terms = 0.2)

  res_fill_2 <- mtcars_cor_pearson_res |>
    fill_safe_value() |>
    show_best_score_prop(prop_terms = 0.8)

  res_fill_transform_1 <- mtcars_cor_pearson_res |>
    fill_safe_value(transform = TRUE) |>
    show_best_score_prop(prop_terms = 0.2)

  res_fill_transform_2 <- mtcars_cor_pearson_res |>
    fill_safe_value(transform = TRUE) |>
    show_best_score_prop(prop_terms = 0.8)

  is_na_score <- is.na(mtcars_cor_pearson_res@results$score)
  mtcars_cor_pearson_res@results$score[
    is_na_score
  ] <- mtcars_cor_pearson_res@fallback_value

  exp_res_1 <- mtcars_cor_pearson_res@results |>
    dplyr::slice_max(score, prop = 0.2)

  exp_res_2 <- mtcars_cor_pearson_res@results |>
    dplyr::slice_max(score, prop = 0.8)

  exp_res_transform_1 <- mtcars_cor_pearson_res@results |>
    dplyr::mutate(score = abs(score)) |>
    dplyr::slice_max(score, prop = 0.2)

  exp_res_transform_2 <- mtcars_cor_pearson_res@results |>
    dplyr::mutate(score = abs(score)) |>
    dplyr::slice_max(score, prop = 0.8)

  expect_equal(res_fill_1$score, exp_res_1$score)
  expect_equal(res_fill_2$score, exp_res_2$score)

  expect_equal(res_fill_transform_1$score, exp_res_transform_1$score)
  expect_equal(res_fill_transform_2$score, exp_res_transform_2$score)
})

test_that("computations - show best score based on prop_terms", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()
  ames_subset <- ames_subset |>
    dplyr::mutate(Sale_Price = log10(Sale_Price))

  ames_aov_pval_res <-
    score_aov_pval |>
    fit(Sale_Price ~ ., data = ames_subset)

  pval_res_1 <- ames_aov_pval_res |> show_best_score_prop(prop_terms = 0.2)
  pval_res_2 <- ames_aov_pval_res |> show_best_score_prop(prop_terms = 0.4)
  pval_res_3 <- ames_aov_pval_res |> show_best_score_prop(prop_terms = 0.6)
  pval_res_4 <- ames_aov_pval_res |> show_best_score_prop(prop_terms = 0.8)

  # ----------------------------------------------------------------------------

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

  # ----------------------------------------------------------------------------
  # TODO direction == "minimize"
})

test_that("computations - show best score based on num_terms", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()
  ames_subset <- ames_subset |>
    dplyr::mutate(Sale_Price = log10(Sale_Price))

  ames_aov_pval_res <-
    score_aov_pval |>
    fit(Sale_Price ~ ., data = ames_subset)

  pval_res_1 <- ames_aov_pval_res |> show_best_score_num(num_terms = 2)
  pval_res_2 <- ames_aov_pval_res |> show_best_score_num(num_terms = 3)
  pval_res_3 <- ames_aov_pval_res |> show_best_score_num(num_terms = 4)

  # ----------------------------------------------------------------------------

  exp_pval_res_1 <- ames_aov_pval_res@results |>
    dplyr::slice_max(score, n = 2)
  exp_pval_res_2 <- ames_aov_pval_res@results |>
    dplyr::slice_max(score, n = 3)
  exp_pval_res_3 <- ames_aov_pval_res@results |>
    dplyr::slice_max(score, n = 4)

  expect_equal(pval_res_1$score, exp_pval_res_1$score)
  expect_equal(pval_res_2$score, exp_pval_res_2$score)
  expect_equal(pval_res_3$score, exp_pval_res_3$score)

  # ----------------------------------------------------------------------------
  # TODO direction == "minimize"
})

test_that("computations - show best score based on cutoff", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()
  ames_subset <- ames_subset |>
    dplyr::mutate(Sale_Price = log10(Sale_Price))

  ames_aov_pval_res <-
    score_aov_pval |>
    fit(Sale_Price ~ ., data = ames_subset)

  pval_res_1 <- ames_aov_pval_res |> show_best_score_cutoff(cutoff = 130)
  pval_res_2 <- ames_aov_pval_res |> show_best_score_cutoff(cutoff = 94.5)

  # ----------------------------------------------------------------------------

  exp_pval_res_1 <- ames_aov_pval_res@results |>
    dplyr::filter(score >= 130)
  exp_pval_res_2 <- ames_aov_pval_res@results |>
    dplyr::filter(score >= 94.5)

  expect_equal(pval_res_1$score, exp_pval_res_1$score)
  expect_equal(pval_res_2$score, exp_pval_res_2$score)

  # ----------------------------------------------------------------------------
  # TODO direction == "minimize"
})

test_that("computations - show best score based on cutoff", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()
  ames_subset <- ames_subset |>
    dplyr::mutate(Sale_Price = log10(Sale_Price))

  ames_aov_pval_res <-
    score_aov_pval |>
    fit(Sale_Price ~ ., data = ames_subset)

  pval_res_1 <- ames_aov_pval_res |> show_best_score_cutoff(cutoff = 130)
  pval_res_2 <- ames_aov_pval_res |> show_best_score_cutoff(cutoff = 94.5)

  # ----------------------------------------------------------------------------

  exp_pval_res_1 <- ames_aov_pval_res@results |>
    dplyr::filter(score >= 130)
  exp_pval_res_2 <- ames_aov_pval_res@results |>
    dplyr::filter(score >= 94.5)

  expect_equal(pval_res_1$score, exp_pval_res_1$score)
  expect_equal(pval_res_2$score, exp_pval_res_2$score)
})

test_that("computations - show best score dual method", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()
  ames_subset <- ames_subset |>
    dplyr::mutate(Sale_Price = log10(Sale_Price))

  ames_aov_pval_res <-
    score_aov_pval |>
    fit(Sale_Price ~ ., data = ames_subset)

  pval_res_1 <- ames_aov_pval_res |> show_best_score_dual(prop_terms = 0.5)

  pval_res_2 <- ames_aov_pval_res |> show_best_score_dual(num_terms = 2)

  # ----------------------------------------------------------------------------

  exp_pval_res_1 <- ames_aov_pval_res |> show_best_score_prop(prop_terms = 0.5)

  exp_pval_res_2 <- ames_aov_pval_res |> show_best_score_num(num_terms = 2)

  expect_equal(pval_res_1$score, exp_pval_res_1$score)
  expect_equal(pval_res_2$score, exp_pval_res_2$score)

  # ----------------------------------------------------------------------------
  # prop_terms used with cutoff

  ames_aov_pval_res <-
    score_aov_pval |>
    fit(Sale_Price ~ ., data = ames_subset)

  pval_res_3 <- ames_aov_pval_res |>
    show_best_score_dual(prop_terms = 0.5, cutoff = 130)

  # ----------------------------------------------------------------------------

  ames_aov_pval_res@results <- ames_aov_pval_res |>
    show_best_score_prop(prop_terms = 0.5)
  exp_pval_res_3 <- ames_aov_pval_res |>
    show_best_score_cutoff(cutoff = 130)

  expect_equal(pval_res_3$score, exp_pval_res_3$score)

  # ----------------------------------------------------------------------------
  # num_terms used with cutoff

  ames_aov_pval_res <-
    score_aov_pval |>
    fit(Sale_Price ~ ., data = ames_subset)

  pval_res_4 <- ames_aov_pval_res |>
    show_best_score_dual(num_terms = 2, cutoff = 130)

  # ----------------------------------------------------------------------------

  ames_aov_pval_res@results <- ames_aov_pval_res |>
    show_best_score_num(num_terms = 2)
  exp_pval_res_4 <- ames_aov_pval_res |>
    show_best_score_cutoff(cutoff = 130)

  expect_equal(pval_res_4$score, exp_pval_res_4$score)
})

test_that("computations - ranking method with gaps", {
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

  pval_res <- ames_aov_pval_res |> filtro::rank_best_score_min()

  fstat_res <- ames_aov_fstat_res |> filtro::rank_best_score_min()

  # ----------------------------------------------------------------------------

  exp_pval_res <- ames_aov_pval_res@results |>
    dplyr::mutate(rank = dplyr::min_rank((dplyr::desc(score))))
  exp_fstat_res <- ames_aov_fstat_res@results |>
    dplyr::mutate(rank = dplyr::min_rank((dplyr::desc(score))))

  expect_equal(pval_res$score, exp_pval_res$score)
  expect_equal(fstat_res$score, exp_fstat_res$score)
})

test_that("computations - ranking method without gaps", {
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

  pval_res <- ames_aov_pval_res |> filtro::rank_best_score_dense()

  fstat_res <- ames_aov_fstat_res |> filtro::rank_best_score_dense()

  # ----------------------------------------------------------------------------

  exp_pval_res <- ames_aov_pval_res@results |>
    dplyr::mutate(rank = dplyr::dense_rank((dplyr::desc(score))))
  exp_fstat_res <- ames_aov_fstat_res@results |>
    dplyr::mutate(rank = dplyr::dense_rank((dplyr::desc(score))))

  expect_equal(pval_res$score, exp_pval_res$score)
  expect_equal(fstat_res$score, exp_fstat_res$score)
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

test_that("computation - bind scores", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()
  ames_subset <- ames_subset |>
    dplyr::mutate(Sale_Price = log10(Sale_Price))

  # anova pval
  ames_aov_pval_res <-
    score_aov_pval |>
    fit(Sale_Price ~ ., data = ames_subset)

  # cor
  ames_cor_pearson_res <-
    score_cor_pearson |>
    fit(Sale_Price ~ ., data = ames_subset)

  # forest imp
  set.seed(42)
  ames_imp_rf_reg_res <-
    score_imp_rf |>
    fit(Sale_Price ~ ., data = ames_subset)

  # info gain
  ames_info_gain_reg_res <-
    score_info_gain |>
    fit(Sale_Price ~ ., data = ames_subset)

  # Create a list
  class_score_list <- list(
    ames_aov_pval_res,
    ames_cor_pearson_res,
    ames_imp_rf_reg_res,
    ames_info_gain_reg_res
  )

  res <- class_score_list |> bind_scores()

  # ----------------------------------------------------------------------------

  length_x <- length(class_score_list)
  score_set <- class_score_list[[1]]@results
  for (i in 2:length_x) {
    score_set <- dplyr::full_join(
      score_set,
      class_score_list[[i]]@results,
      by = c("name", "score", "outcome", "predictor")
    )
  }

  score_set <- score_set |>
    tidyr::pivot_wider(names_from = name, values_from = score)
  exp_res <- score_set

  expect_equal(res$aov_pval, exp_res$aov_pval)
  expect_equal(res$cor_pearson, exp_res$cor_pearson)
  expect_equal(res$imp_rf, exp_res$imp_rf)
  expect_equal(res$infogain, exp_res$infogain)

  # ----------------------------------------------------------------------------
  # Single score

  # cor
  ames_cor_pearson_res <-
    score_cor_pearson |>
    fit(Sale_Price ~ ., data = ames_subset)

  # Create a list
  class_score_list <- list(
    ames_cor_pearson_res
  )

  res_single <- class_score_list |> bind_scores()

  expect_named(res_single, c("outcome", "predictor", "cor_pearson"))
})

test_that("computation - fill safe values", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()
  ames_subset <- ames_subset |>
    dplyr::mutate(Sale_Price = log10(Sale_Price))

  # anova pval
  ames_aov_pval_res <-
    score_aov_pval |>
    fit(Sale_Price ~ ., data = ames_subset)

  # cor
  ames_cor_pearson_res <-
    score_cor_pearson |>
    fit(Sale_Price ~ ., data = ames_subset)

  # forest imp
  set.seed(42)
  ames_imp_rf_reg_res <-
    score_imp_rf |>
    fit(Sale_Price ~ ., data = ames_subset)

  # info gain
  ames_info_gain_reg_res <-
    score_info_gain |>
    fit(Sale_Price ~ ., data = ames_subset)

  # Create a list
  class_score_list <- list(
    ames_aov_pval_res,
    ames_cor_pearson_res,
    ames_imp_rf_reg_res,
    ames_info_gain_reg_res
  )

  res <- class_score_list |> fill_safe_values()

  # ----------------------------------------------------------------------------

  score_set <- bind_scores(class_score_list)
  for (i in 1:length(class_score_list)) {
    method_name <- class_score_list[[i]]@score_type
    fallback_val <- class_score_list[[i]]@fallback_value
    is_na_score <- is.na(score_set[[method_name]])
    score_set[[method_name]][is_na_score] <- fallback_val
  }
  exp_res <- score_set

  expect_equal(res$aov_pval, exp_res$aov_pval)
  expect_equal(res$cor_pearson, exp_res$cor_pearson)
  expect_equal(res$imp_rf, exp_res$imp_rf)
  expect_equal(res$infogain, exp_res$infogain)

  # ----------------------------------------------------------------------------
  # Single score

  # cor
  ames_cor_pearson_res <-
    score_cor_pearson |>
    fit(Sale_Price ~ ., data = ames_subset)

  # Create a list
  class_score_list <- list(
    ames_cor_pearson_res
  )

  res_single <- class_score_list |> fill_safe_values()

  expect_named(res_single, c("outcome", "predictor", "cor_pearson"))
})

test_that("computation - fill safe values adding transformation", {
  skip_if_not_installed("modeldata")

  # anova pval
  mtcars_aov_pval_res <-
    score_aov_pval |>
    fit(mpg ~ ., data = mtcars)

  # cor
  mtcars_cor_pearson_res <-
    score_cor_pearson |>
    fit(mpg ~ ., data = mtcars)

  # Create a list
  class_score_list <- list(
    mtcars_aov_pval_res,
    mtcars_cor_pearson_res
  )

  res <- class_score_list |> fill_safe_values()

  res_transform <- class_score_list |> fill_safe_values(transform = TRUE)

  # ----------------------------------------------------------------------------

  score_set <- bind_scores(class_score_list)
  for (i in 1:length(class_score_list)) {
    method_name <- class_score_list[[i]]@score_type
    fallback_val <- class_score_list[[i]]@fallback_value
    is_na_score <- is.na(score_set[[method_name]])
    score_set[[method_name]][is_na_score] <- fallback_val
  }
  exp_res <- score_set

  expect_equal(res$aov_pval, exp_res$aov_pval)
  expect_equal(res$cor_pearson, exp_res$cor_pearson)

  expect_equal(res_transform$aov_pval, exp_res$aov_pval)
  expect_equal(res_transform$cor_pearson, abs(exp_res$cor_pearson))

  # ----------------------------------------------------------------------------
  # Single score

  # cor
  mtcars_cor_pearson_res <-
    score_cor_pearson |>
    fit(mpg ~ ., data = mtcars)

  # Create a list
  class_score_list <- list(
    mtcars_cor_pearson_res
  )

  res_single <- class_score_list |> fill_safe_values()
  res_single_transform <- class_score_list |> fill_safe_values(transform = TRUE)

  expect_named(res_single, c("outcome", "predictor", "cor_pearson"))
  expect_named(res_single_transform, c("outcome", "predictor", "cor_pearson"))
})

# TODO Some tests are not exhaustive and can be improved
