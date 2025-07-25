# Arrange score

ames_subset <- helper_ames()
ames_subset <- ames_subset |>
  dplyr::mutate(Sale_Price = log10(Sale_Price))

ames_aov_pval_res <-
  score_aov_pval |>
  fit(Sale_Price ~ ., data = ames_subset)
ames_aov_pval_res@results
ames_aov_pval_res |>
  filtro::arrange_score()

ames_aov_fstat_res <-
  score_aov_fstat |>
  fit(Sale_Price ~ ., data = ames_subset)
ames_aov_fstat_res@results
ames_aov_fstat_res |>
  filtro::arrange_score()

# Fill safe value
ames_aov_pval_res <-
  score_aov_pval |>
  fit(Sale_Price ~ ., data = ames_subset)
ames_aov_pval_res@results
ames_aov_pval_res |>
  filtro::fill_safe_value()

ames_aov_fstat_res <-
  score_aov_fstat |>
  fit(Sale_Price ~ ., data = ames_subset)
ames_aov_fstat_res@results
ames_aov_fstat_res |>
  filtro::fill_safe_value()


# Show best score based on based on proportion of predictors

ames_subset <- helper_ames()
ames_subset <- ames_subset |>
  dplyr::mutate(Sale_Price = log10(Sale_Price))

ames_aov_pval_res@results
ames_aov_pval_res |>
  filtro::show_best_score_prop(prop_terms = 0.2)

ames_aov_fstat_res@results
ames_aov_fstat_res |>
  filtro::show_best_score_prop(prop_terms = 0.2)

# Show best score based on number of predictors

ames_subset <- helper_ames()
ames_subset <- ames_subset |>
  dplyr::mutate(Sale_Price = log10(Sale_Price))

ames_aov_pval_res@results
ames_aov_pval_res |>
  filtro::show_best_score_num(num_terms = 2)

ames_aov_fstat_res@results
ames_aov_fstat_res |>
  filtro::show_best_score_num(num_terms = 2)

# Show best score based on cutoff value

ames_subset <- helper_ames()
ames_subset <- ames_subset |>
  dplyr::mutate(Sale_Price = log10(Sale_Price))

ames_aov_pval_res@results
ames_aov_pval_res |>
  filtro::show_best_score_cutoff(cutoff = 130)

ames_aov_fstat_res@results
ames_aov_fstat_res |>
  filtro::show_best_score_cutoff(cutoff = 94.5)

# Show best score based on proportion of predictors with
# optional cutoff value

ames_subset <- helper_ames()
ames_subset <- ames_subset |>
  dplyr::mutate(Sale_Price = log10(Sale_Price))

ames_aov_pval_res@results
ames_aov_pval_res |>
  filtro::show_best_score_dual(prop_terms = 0.5)
ames_aov_pval_res |>
  filtro::show_best_score_dual(prop_terms = 0.5, cutoff = 130)

ames_aov_pval_res@results
ames_aov_pval_res |>
  filtro::show_best_score_dual(num_terms = 2)
ames_aov_pval_res |>
  filtro::show_best_score_dual(prop_terms = 2, cutoff = 130)

# Rank score based on min_rank()
ames_subset <- helper_ames()
ames_subset <- ames_subset |>
  dplyr::mutate(Sale_Price = log10(Sale_Price))

ames_aov_pval_res@results
ames_aov_pval_res |> filtro::rank_best_score_min()

ames_aov_fstat_res@results
ames_aov_fstat_res |> filtro::rank_best_score_min()

# Rank score based on dense_rank()
ames_subset <- helper_ames()
ames_subset <- ames_subset |>
  dplyr::mutate(Sale_Price = log10(Sale_Price))

ames_aov_pval_res@results
ames_aov_pval_res |> filtro::rank_best_score_dense()

ames_aov_fstat_res@results
ames_aov_fstat_res |> filtro::rank_best_score_dense()

# Construct an S7 subclass of base R's `list` for Method Dispatch
# Bind all `class_score` objects, including their associated metadata and scores
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
score_imp_rf_reg <- score_imp_rf
score_imp_rf_reg@mode <- "regression"
set.seed(42)
ames_imp_rf_reg_res <-
  score_imp_rf_reg |>
  fit(Sale_Price ~ ., data = ames_subset)
ames_imp_rf_reg_res@results

# info gain
score_info_gain_reg <- score_info_gain
score_info_gain_reg@mode <- "regression"

ames_info_gain_reg_res <-
  score_info_gain_reg |>
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


skip()

test_that("arrange_score() is working for aov", {
  skip("refactor arrange code for aov objects")
  ames_subset <- helper_ames()
  ames_subset <- ames_subset |>
    dplyr::mutate(Sale_Price = log10(Sale_Price))

  # TODO Right now user have to run > 3 lines to re-arrange score.
  # This is because results is now a propertity in new_score_obj,
  # and re-running score_aov() clears score_res.

  score_obj <- filtro::score_aov(direction = "maximize")
  score_res <- filtro::get_scores_aov(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )
  ex.max <- score_obj |>
    filtro::attach_score(results = score_res) |>
    filtro::arrange_score()

  score_obj <- filtro::score_aov(direction = "minimize")
  score_res <- filtro::get_scores_aov(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )
  ex.min <- score_obj |>
    filtro::attach_score(results = score_res) |>
    filtro::arrange_score()

  score_obj <- filtro::score_aov(direction = "target")
  score_res <- filtro::get_scores_aov(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )
  ex.target <- score_obj |>
    filtro::attach_score(results = score_res) |>
    filtro::arrange_score(target = 94.4)

  score_obj <- filtro::score_aov(direction = "target")
  score_res <- filtro::get_scores_aov(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )
  ex.target2 <- score_obj |>
    filtro::attach_score(results = score_res) |>
    filtro::arrange_score(target = 22.8)

  expect_equal(ex.max, score_res |> dplyr::arrange(desc(score)))

  expect_equal(ex.min, score_res |> dplyr::arrange(score))

  expect_equal(ex.target, score_res |> dplyr::arrange(abs(score - 94.4)))

  expect_equal(ex.target2, score_res |> dplyr::arrange(abs(score - 22.8)))
})

# test_that("trans_score() is working for aov", {
#   # skip_if_not_installed("modeldata")
#   # data(ames, package = "modeldata")
#   # data <- modeldata::ames |>
#   #   dplyr::select(
#   #     Sale_Price,
#   #     MS_SubClass,
#   #     MS_Zoning,
#   #     Lot_Frontage,
#   #     Lot_Area,
#   #     Street
#   #   )
#   # outcome <- "Sale_Price"
#   # score_obj = score_aov()
#   # score_res <- get_scores_aov(score_obj, data, outcome)
#   # score_obj <- score_obj |> attach_score(score_res)

#   score_obj <- ames_score_obj()
#   score_res <- score_obj$results

#   score_obj$trans <- NULL # Default
#   ex.identity <- score_obj |> trans_score()

#   score_obj$trans <- scales::transform_log()
#   ex.log <- score_obj |> trans_score()

#   # TODO Add a couple more scales::transform_*()

#   expect_equal(ex.identity, score_res)

#   expect_equal(ex.log, score_res |> dplyr::mutate(score = log(score)))
# })

test_that("filter_score_num() is working for aov", {
  skip("refactor arrange code for aov objects")
  ames_subset <- helper_ames()
  ames_subset <- ames_subset |>
    dplyr::mutate(Sale_Price = log10(Sale_Price))

  score_obj <- filtro::score_aov(direction = "maximize")
  score_res <- filtro::get_scores_aov(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )
  ex.max <- score_obj |>
    filtro::attach_score(results = score_res) |>
    filter_score_num(num_terms = 2)

  score_obj <- filtro::score_aov(direction = "minimize")
  score_res <- filtro::get_scores_aov(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )
  ex.min <- score_obj |>
    filtro::attach_score(results = score_res) |>
    filter_score_num(num_terms = 2)

  score_obj <- filtro::score_aov(direction = "target")
  score_res <- filtro::get_scores_aov(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )
  ex.target <- score_obj |>
    filtro::attach_score(results = score_res) |>
    filter_score_num(num_terms = 2, target = 94.4)

  score_obj <- filtro::score_aov(direction = "target")
  score_res <- filtro::get_scores_aov(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )
  ex.target2 <- score_obj |>
    filtro::attach_score(results = score_res) |>
    filter_score_num(num_terms = 2, target = 22.8)

  expect_equal(ex.max, score_res |> dplyr::slice_max(score, n = 2))

  expect_equal(ex.min, score_res |> dplyr::slice_min(score, n = 2))

  expect_equal(
    ex.target,
    score_res |>
      dplyr::arrange(abs(score - 94.4)) |>
      dplyr::slice_head(n = 2)
  )

  expect_equal(
    ex.target2,
    score_res |>
      dplyr::arrange(abs(score - 22.8)) |>
      dplyr::slice_head(n = 2)
  )
})

test_that("filter_score_prop() is working for aov", {
  skip("refactor arrange code for aov objects")
  ames_subset <- helper_ames()
  ames_subset <- ames_subset |>
    dplyr::mutate(Sale_Price = log10(Sale_Price))

  score_obj <- filtro::score_aov(direction = "maximize")
  score_res <- filtro::get_scores_aov(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )
  ex.max <- score_obj |>
    filtro::attach_score(results = score_res) |>
    filter_score_prop(prop_terms = 0.2)

  score_obj <- filtro::score_aov(direction = "minimize")
  score_res <- filtro::get_scores_aov(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )
  ex.min <- score_obj |>
    filtro::attach_score(results = score_res) |>
    filter_score_prop(prop_terms = 0.2)

  score_obj <- filtro::score_aov(direction = "target")
  score_res <- filtro::get_scores_aov(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )
  ex.target <- score_obj |>
    filtro::attach_score(results = score_res) |>
    filter_score_prop(prop_terms = 0.2, target = 94.4)

  score_obj <- filtro::score_aov(direction = "target")
  score_res <- filtro::get_scores_aov(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )
  ex.target2 <- score_obj |>
    filtro::attach_score(results = score_res) |>
    filter_score_prop(prop_terms = 0.2, target = 22.8)

  expect_equal(ex.max, score_res |> dplyr::slice_max(score, prop = 0.2))

  expect_equal(ex.min, score_res |> dplyr::slice_min(score, prop = 0.2))

  expect_equal(
    ex.target,
    score_res |>
      dplyr::arrange(abs(score - 94.4)) |>
      dplyr::slice_head(prop = 0.2)
  )

  expect_equal(
    ex.target2,
    score_res |>
      dplyr::arrange(abs(score - 22.8)) |>
      dplyr::slice_head(prop = 0.2)
  )
})

test_that("filter_score_cutoff() is working for aov", {
  skip("refactor arrange code for aov objects")
  ames_subset <- helper_ames()
  ames_subset <- ames_subset |>
    dplyr::mutate(Sale_Price = log10(Sale_Price))

  score_obj <- filtro::score_aov(direction = "maximize")
  score_res <- filtro::get_scores_aov(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )
  ex.max <- score_obj |>
    filtro::attach_score(results = score_res) |>
    filter_score_cutoff(cutoff = 94.4)

  score_obj <- filtro::score_aov(direction = "minimize")
  score_res <- filtro::get_scores_aov(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )
  ex.min <- score_obj |>
    filtro::attach_score(results = score_res) |>
    filter_score_cutoff(cutoff = 94.4)

  score_obj <- filtro::score_aov(direction = "target")
  score_res <- filtro::get_scores_aov(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )
  ex.target <- score_obj |>
    filtro::attach_score(results = score_res) |>
    filter_score_cutoff(target = 94.4, cutoff = 4)

  ex.target2 <- score_obj |>
    filtro::attach_score(results = score_res) |>
    filter_score_cutoff(target = 22.8, cutoff = 1)

  expect_equal(
    ex.max,
    score_res |> dplyr::filter(score >= 94.4)
  )

  expect_equal(
    ex.min,
    score_res |> dplyr::filter(score <= 94.4)
  )

  expect_equal(
    ex.target,
    score_res |> dplyr::filter(abs(score - 94.4) <= 4)
  )

  expect_equal(
    ex.target2,
    score_res |> dplyr::filter(abs(score - 22.8) <= 1)
  )
})

test_that("filter_score_auto() is working for aov", {
  skip("refactor arrange code for aov objects")
  ames_subset <- helper_ames()
  ames_subset <- ames_subset |>
    dplyr::mutate(Sale_Price = log10(Sale_Price))

  score_obj <- filtro::score_aov(direction = "maximize")
  score_res <- filtro::get_scores_aov(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )
  ex.max <- score_obj |>
    filtro::attach_score(results = score_res) |>
    filter_score_auto(num_terms = 2)
  ex.max2 <- score_obj |>
    filtro::attach_score(results = score_res) |>
    filter_score_auto(num_terms = 2, cutoff = 94.6)
  ex.max3 <- score_obj |>
    filtro::attach_score(results = score_res) |>
    filter_score_auto(prop_terms = 0.5)
  ex.max4 <- score_obj |>
    filtro::attach_score(results = score_res) |>
    filter_score_auto(prop_terms = 0.5, cutoff = 94.6)

  expect_equal(
    ex.max,
    score_obj |>
      filtro::attach_score(results = score_res) |>
      filter_score_num(num_terms = 2)
  )

  expect_equal(
    ex.max2,
    {
      score_res2 <- score_obj |>
        filtro::attach_score(results = score_res) |>
        filter_score_num(num_terms = 2)
      score_obj |>
        filtro::attach_score(results = score_res2) |>
        filter_score_cutoff(cutoff = 94.6)
    }
  )

  expect_equal(
    ex.max3,
    score_obj |>
      filtro::attach_score(results = score_res) |>
      filter_score_prop(prop_terms = 0.5)
  )

  expect_equal(
    ex.max4,
    {
      score_res4 <- score_obj |>
        filtro::attach_score(results = score_res) |>
        filter_score_prop(prop_terms = 0.5)
      score_obj |>
        filtro::attach_score(results = score_res4) |>
        filter_score_cutoff(cutoff = 94.6)
    }
  )
  # TODO Finish direction = "minimize"
  # TODO Finish direction = "target"
})

# TODO Test rank_score_min()

# TODO Test rank_score_dense()

# score_res %>%
#   dplyr::mutate(rank = dplyr::min_rank((dplyr::desc(score))))

# score_res %>%
#   dplyr::mutate(rank = dplyr::min_rank((score)))

# score_res %>%
#   dplyr::mutate(rank = dplyr::dense_rank((dplyr::desc(score))))

# score_res %>%
#   dplyr::mutate(rank = dplyr::dense_rank((score)))

test_that("bind_scores() is working for aov", {
  skip("refactor arrange code for aov objects")
  ames_subset <- helper_ames()
  ames_subset <- ames_subset |>
    dplyr::mutate(Sale_Price = log10(Sale_Price))

  score_obj_aov <- filtro::score_aov()
  score_res_aov <- filtro::get_scores_aov(
    score_obj_aov,
    data = ames_subset,
    outcome = "Sale_Price"
  )
  score_obj_aov <- score_obj_aov |> attach_score(score_res_aov)

  score_obj_cor <- filtro::score_cor()
  score_res_cor <- filtro::get_scores_cor(
    score_obj_cor,
    data = ames_subset,
    outcome = "Sale_Price"
  )
  score_obj_cor <- score_obj_cor |> filtro::attach_score(score_res_cor)

  score_obj_imp <- filtro::score_forest_imp(mode = "regression")
  score_res_imp <- get_scores_forest_importance(
    score_obj_imp,
    data = ames_subset,
    outcome = "Sale_Price"
  )
  score_obj_imp <- score_obj_imp |> filtro::attach_score(score_res_imp)

  score_obj_info <- score_info_gain(mode = "regression")
  score_res_info <- get_scores_info_gain(
    score_obj_info,
    data = ames_subset,
    outcome = "Sale_Price"
  )
  score_obj_info <- score_obj_info |> filtro::attach_score(score_res_info)

  score_obj_list <- list(
    score_obj_aov,
    score_obj_cor,
    score_obj_imp,
    score_obj_info
  )

  ex.res <- score_obj_list |> filtro::bind_scores()

  expect_equal(ex.res, {
    score_set <- score_obj_list[[1]]@results
    for (i in 2:length(score_obj_list)) {
      next_results <- score_obj_list[[i]]@results
      score_set <- dplyr::full_join(
        score_set,
        next_results,
        by = c("name", "score", "outcome", "predictor")
      )
    }
    score_set |>
      tidyr::pivot_wider(names_from = name, values_from = score)
  })
})

test_that("fill_safe_values() is working for aov", {
  skip("refactor arrange code for aov objects")
  ames_subset <- helper_ames()
  ames_subset <- ames_subset |>
    dplyr::mutate(Sale_Price = log10(Sale_Price))

  score_obj_aov <- filtro::score_aov()
  score_res_aov <- filtro::get_scores_aov(
    score_obj_aov,
    data = ames_subset,
    outcome = "Sale_Price"
  )
  score_obj_aov <- score_obj_aov |> attach_score(score_res_aov)

  score_obj_cor <- filtro::score_cor()
  score_res_cor <- filtro::get_scores_cor(
    score_obj_cor,
    data = ames_subset,
    outcome = "Sale_Price"
  )
  score_obj_cor <- score_obj_cor |> filtro::attach_score(score_res_cor)

  score_obj_imp <- filtro::score_forest_imp(mode = "regression")
  score_res_imp <- get_scores_forest_importance(
    score_obj_imp,
    data = ames_subset,
    outcome = "Sale_Price"
  )
  score_obj_imp <- score_obj_imp |> filtro::attach_score(score_res_imp)

  score_obj_info <- score_info_gain(mode = "regression")
  score_res_info <- get_scores_info_gain(
    score_obj_info,
    data = ames_subset,
    outcome = "Sale_Price"
  )
  score_obj_info <- score_obj_info |> filtro::attach_score(score_res_info)

  score_obj_list <- list(
    score_obj_aov,
    score_obj_cor,
    score_obj_imp,
    score_obj_info
  )

  ex.res <- score_obj_list |> filtro::fill_safe_values()

  expect_equal(ex.res, {
    score_set <- score_obj_list |> filtro::bind_scores()
    for (i in 1:length(score_obj_list)) {
      method_name <- score_obj_list[[i]]@score_type
      fallback_val <- score_obj_list[[i]]@fallback_value
      score_set[[method_name]][is.na(score_set[[method_name]])] <- fallback_val
    }
    score_set
  })
})

# TODO May need to test for methods other than aov
