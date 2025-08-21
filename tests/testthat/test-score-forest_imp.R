test_that("object creation", {
  expect_s3_class(
    score_imp_rf,
    c("filtro::class_score_imp_rf", "filtro::class_score", "S7_object")
  )

  expect_s3_class(
    score_imp_rf_conditional,
    c("filtro::class_score_imp_rf", "filtro::class_score", "S7_object")
  )

  expect_s3_class(
    score_imp_rf_oblique,
    c("filtro::class_score_imp_rf", "filtro::class_score", "S7_object")
  )
})

# ------------------------------------------------------------------------------

test_that("updating ranger args", {
  before_1 <- list()
  after_1 <- convert_rf_args(before_1, method = "ranger")
  expect_equal(before_1, after_1)

  before_2 <- list(mtry = 5)
  after_2 <- convert_rf_args(before_2, method = "ranger")
  expect_equal(after_2, before_2)

  # leave engine/original args alone
  before_3 <- list(trees = 5, regularization.factor = 1 / 2, min.node.size = 1)
  after_3 <- convert_rf_args(before_3, method = "ranger")
  expect_equal(
    after_3,
    list(num.trees = 5, regularization.factor = 0.5, min.node.size = 1)
  )

  # All conversions
  before_4 <- list(trees = 5, min_n = 1, mtry = 3)
  after_4 <- convert_rf_args(before_4, method = "ranger")
  expect_equal(
    after_4,
    list(num.trees = 5, min.node.size = 1, mtry = 3)
  )
})

test_that("updating cforest args", {
  # All conversions
  before_1 <- list(trees = 5, min_n = 1, mtry = 3)
  after_1 <- convert_rf_args(before_1, method = "partykit")
  expect_equal(
    after_1,
    list(ntree = 5, minsplit = 1, mtry = 3)
  )
})

test_that("updating cforest args", {
  # All conversions
  before_1 <- list(trees = 5, min_n = 1, mtry = 3)
  after_1 <- convert_rf_args(before_1, method = "aorsf")
  expect_equal(
    after_1,
    list(n_tree = 5, leaf_min_obs = 1, mtry = 3)
  )
})

test_that("updating with default args", {
  expect_equal(
    update_defaults(list(a = 1, b = 2), list()),
    list(a = 1, b = 2)
  )

  expect_equal(
    update_defaults(list(a = 1, b = 2), list(a = 3)),
    list(a = 1, b = 2)
  )

  expect_equal(
    update_defaults(list(a = 1, b = 2), list(c = 3)),
    list(c = 3, a = 1, b = 2)
  )
})

# ------------------------------------------------------------------------------

test_that("computations - classification task via ranger", {
  skip_if_not_installed("modeldata")

  cells_subset <- helper_cells()

  cells_imp_rf_res <- score_imp_rf |>
    fit(
      class ~ .,
      data = cells_subset,
      trees = 100,
      mtry = 2,
      seed = 42
    )

  # ----------------------------------------------------------------------------

  y <- cells_subset[["class"]]
  X <- cells_subset[setdiff(names(cells_subset), "class")]
  fit_ranger <- ranger::ranger(
    y = y,
    x = X,
    num.trees = 100,
    mtry = 2,
    importance = "permutation",
    seed = 42
  )
  imp_ranger <- (fit_ranger$variable.importance) |> unname()

  expect_equal(cells_imp_rf_res@results$score, imp_ranger)

  # ----------------------------------------------------------------------------

  expect_equal(cells_imp_rf_res@range, c(0.0, Inf))
  expect_equal(cells_imp_rf_res@inclusive, rep(FALSE, 2))
  expect_equal(cells_imp_rf_res@fallback_value, Inf)
  expect_equal(cells_imp_rf_res@direction, "maximize")
})

test_that("computations - regression task via ranger", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()
  ames_subset <- ames_subset |>
    dplyr::mutate(Sale_Price = log10(Sale_Price))

  ames_imp_rf_regression_task_res <-
    score_imp_rf |>
    fit(
      Sale_Price ~ .,
      data = ames_subset,
      seed = 42,
      trees = 100,
      mtry = 2
    )

  # ----------------------------------------------------------------------------

  y <- ames_subset[["Sale_Price"]]
  X <- ames_subset[setdiff(names(ames_subset), "Sale_Price")]
  fit_ranger <- ranger::ranger(
    y = y,
    x = X,
    num.trees = 100,
    mtry = 2,
    importance = "permutation",
    seed = 42
  )
  imp_ranger <- (fit_ranger$variable.importance) |> unname()

  expect_equal(ames_imp_rf_regression_task_res@results$score, imp_ranger)

  # ----------------------------------------------------------------------------

  expect_equal(ames_imp_rf_regression_task_res@range, c(0.0, Inf))
  expect_equal(ames_imp_rf_regression_task_res@inclusive, rep(FALSE, 2))
  expect_equal(ames_imp_rf_regression_task_res@fallback_value, Inf)
  expect_equal(ames_imp_rf_regression_task_res@direction, "maximize")
})

test_that("computations - regression task via ranger vary trees, mtry, min_n", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()
  ames_subset <- ames_subset |>
    dplyr::mutate(Sale_Price = log10(Sale_Price))

  ames_imp_rf_regression_task_res <-
    score_imp_rf |>
    fit(
      Sale_Price ~ .,
      data = ames_subset,
      trees = 50,
      mtry = 1,
      min_n = 2,
      seed = 42
    )

  # ----------------------------------------------------------------------------

  y <- ames_subset[["Sale_Price"]]
  X <- ames_subset[setdiff(names(ames_subset), "Sale_Price")]
  fit_ranger <- ranger::ranger(
    y = y,
    x = X,
    num.trees = 50,
    mtry = 1,
    importance = "permutation",
    min.node.size = 2,
    classification = FALSE,
    seed = 42
  )
  imp_ranger <- (fit_ranger$variable.importance) |> unname()

  expect_equal(ames_imp_rf_regression_task_res@results$score, imp_ranger)

  # ----------------------------------------------------------------------------

  expect_equal(ames_imp_rf_regression_task_res@range, c(0.0, Inf))
  expect_equal(ames_imp_rf_regression_task_res@inclusive, rep(FALSE, 2))
  expect_equal(ames_imp_rf_regression_task_res@fallback_value, Inf)
  expect_equal(ames_imp_rf_regression_task_res@direction, "maximize")
})

# ------------------------------------------------------------------------------

test_that("computations - classification task via partykit", {
  skip_if_not_installed("modeldata")
  cells_subset <- helper_cells() |> dplyr::slice(1:200)

  set.seed(42)
  cells_imp_rf_conditional_res <- score_imp_rf_conditional |>
    fit(class ~ ., data = cells_subset, trees = 50)

  # ----------------------------------------------------------------------------

  # Repeatedly find different results than the fit() call above
  set.seed(42)
  fit_partykit <- partykit::cforest(
    formula = class ~ .,
    data = cells_subset,
    ntree = 50
  )
  imp_partykit_raw <- partykit::varimp(fit_partykit, conditional = TRUE)
  imp_partykit_raw <-
    tibble::enframe(imp_partykit_raw, name = "predictor", value = "raw")

  imp_partykit_merge <-
    cells_imp_rf_conditional_res@results |>
    dplyr::full_join(imp_partykit_raw, by = "predictor")

  expect_equal(
    rank(imp_partykit_merge$score),
    rank(imp_partykit_merge$raw),
    tolerance = 0.1
  )

  # ----------------------------------------------------------------------------

  expect_equal(cells_imp_rf_conditional_res@range, c(0.0, Inf))
  expect_equal(cells_imp_rf_conditional_res@inclusive, rep(FALSE, 2))
  expect_equal(cells_imp_rf_conditional_res@fallback_value, Inf)
  expect_equal(cells_imp_rf_conditional_res@direction, "maximize")
})

test_that("computations - regression task via partykit", {
  skip_if_not_installed("modeldata")

  set.seed(1)
  ames_subset <-
    helper_ames() |>
    dplyr::slice_sample(n = 50, by = Street) |>
    dplyr::select(Sale_Price, Lot_Area, MS_Zoning)

  ames_subset <- ames_subset |>
    dplyr::mutate(Sale_Price = log10(Sale_Price))

  set.seed(42)
  ames_imp_rf_conditional_res <- score_imp_rf_conditional |>
    fit(Sale_Price ~ ., data = ames_subset, trees = 50)

  # ----------------------------------------------------------------------------

  set.seed(42)
  fit_partykit <- partykit::cforest(
    formula = Sale_Price ~ .,
    data = ames_subset,
    ntree = 50
  )
  imp_partykit_raw <- partykit::varimp(fit_partykit, conditional = TRUE)
  imp_partykit_raw <-
    tibble::enframe(imp_partykit_raw, name = "predictor", value = "raw")

  imp_partykit_merge <-
    ames_imp_rf_conditional_res@results |>
    dplyr::full_join(imp_partykit_raw, by = "predictor")

  expect_equal(
    rank(imp_partykit_merge$score),
    rank(imp_partykit_merge$raw),
    tolerance = 0.1
  )

  # ----------------------------------------------------------------------------

  expect_equal(ames_imp_rf_conditional_res@range, c(0.0, Inf))
  expect_equal(ames_imp_rf_conditional_res@inclusive, rep(FALSE, 2))
  expect_equal(ames_imp_rf_conditional_res@fallback_value, Inf)
  expect_equal(ames_imp_rf_conditional_res@direction, "maximize")
})

# ------------------------------------------------------------------------------

test_that("computations - classification task via aorsf", {
  skip_if_not_installed("modeldata")

  cells_subset <- helper_cells()

  set.seed(42)
  cells_imp_rf_oblique_res <- score_imp_rf_oblique |>
    fit(class ~ ., data = cells_subset, trees = 100, n_retry = 2)

  # ----------------------------------------------------------------------------

  set.seed(42)
  fit_aorsf <- aorsf::orsf(
    formula = class ~ .,
    data = cells_subset,
    n_tree = 100,
    n_retry = 2,
    importance = "permute"
  )
  imp_aorsf_raw <-
    tibble::enframe(fit_aorsf$importance, name = "predictor", value = "raw")

  imp_aorsf_merge <-
    cells_imp_rf_oblique_res@results |>
    dplyr::full_join(imp_aorsf_raw, by = "predictor")

  expect_equal(
    imp_aorsf_merge$score,
    imp_aorsf_merge$raw,
    tolerance = 0.1
  )

  # ----------------------------------------------------------------------------

  expect_equal(cells_imp_rf_oblique_res@range, c(0.0, Inf))
  expect_equal(cells_imp_rf_oblique_res@inclusive, rep(FALSE, 2))
  expect_equal(cells_imp_rf_oblique_res@fallback_value, Inf)
  expect_equal(cells_imp_rf_oblique_res@direction, "maximize")
})

test_that("computations - regression task via aorsf", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()
  ames_subset <- ames_subset |>
    dplyr::mutate(Sale_Price = log10(Sale_Price))

  set.seed(42)
  ames_imp_rf_oblique_res <- score_imp_rf_oblique |>
    fit(Sale_Price ~ ., data = ames_subset, n_tree = 100, n_retry = 2)

  # ----------------------------------------------------------------------------

  set.seed(42)
  fit_aorsf <- aorsf::orsf(
    formula = Sale_Price ~ .,
    data = ames_subset,
    n_tree = 100,
    n_retry = 2,
    importance = "permute"
  )
  imp_aorsf_raw <-
    tibble::enframe(fit_aorsf$importance, name = "predictor", value = "raw")

  imp_aorsf_merge <-
    ames_imp_rf_oblique_res@results |>
    dplyr::full_join(imp_aorsf_raw, by = "predictor")

  expect_equal(
    imp_aorsf_merge$score,
    imp_aorsf_merge$raw,
    tolerance = 0.1
  )

  # ----------------------------------------------------------------------------

  expect_equal(ames_imp_rf_oblique_res@range, c(0.0, Inf))
  expect_equal(ames_imp_rf_oblique_res@inclusive, rep(FALSE, 2))
  expect_equal(ames_imp_rf_oblique_res@fallback_value, Inf)
  expect_equal(ames_imp_rf_oblique_res@direction, "maximize")
})

test_that("computations - regression task via aorsf - adding missing values and case weights", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()
  ames_subset <- ames_subset |>
    dplyr::mutate(Sale_Price = log10(Sale_Price))

  # ----------------------------------------------------------------------------
  # missing values

  ames_missing <- ames_subset
  ames_missing$Sale_Price[1] <- NA_real_
  ames_missing$Lot_Frontage[2] <- NA_real_

  set.seed(42)
  ames_missing_imp_rf_oblique_res <- score_imp_rf_oblique |>
    fit(Sale_Price ~ ., data = ames_missing)

  # ----------------------------------------------------------------------------

  ames_missing <- ames_missing[stats::complete.cases(ames_missing), ]

  set.seed(42)
  fit_aorsf <- aorsf::orsf(
    formula = Sale_Price ~ .,
    data = ames_missing,
    importance = "permute"
  )
  imp_aorsf_raw <-
    tibble::enframe(fit_aorsf$importance, name = "predictor", value = "raw")

  imp_aorsf_merge <-
    ames_missing_imp_rf_oblique_res@results |>
    dplyr::full_join(imp_aorsf_raw, by = "predictor")

  expect_equal(
    imp_aorsf_merge$score,
    imp_aorsf_merge$raw,
    tolerance = 0.1
  )

  # ----------------------------------------------------------------------------
  # case weights
  two_weights <- seq(0, 1, length.out = nrow(ames_subset))

  set.seed(42)
  ames_weights_imp_rf_oblique_res <- score_imp_rf_oblique |>
    fit(Sale_Price ~ ., data = ames_subset, case_weights = two_weights)

  # ----------------------------------------------------------------------------

  set.seed(42)
  fit_aorsf <- aorsf::orsf(
    formula = Sale_Price ~ .,
    data = ames_subset,
    importance = "permute",
    weights = two_weights
  )
  imp_aorsf_raw <-
    tibble::enframe(fit_aorsf$importance, name = "predictor", value = "raw")

  imp_aorsf_merge <-
    ames_weights_imp_rf_oblique_res@results |>
    dplyr::full_join(imp_aorsf_raw, by = "predictor")

  # We find numerical differences across OSes
  skip_on_os("linux")
  skip_on_os("windows")

  expect_equal(
    imp_aorsf_merge$score,
    imp_aorsf_merge$raw,
    tolerance = 0.1
  )
})


test_that("computations - regression task via aorsf - zero variance predictors", {
  skip_if_not_installed("modeldata")

  zv_data <- mtcars
  zv_data$potato <- rep(1.0, 32)
  zv_data$kartoffel <- factor(rep("a", 32))

  set.seed(42)
  zv_imp_rf_oblique_res <- score_imp_rf_oblique |>
    fit(mpg ~ ., data = zv_data)

  # ----------------------------------------------------------------------------

  set.seed(42)
  fit_aorsf <- aorsf::orsf(
    formula = mpg ~ .,
    data = zv_data[, 1:ncol(mtcars)],
    importance = "permute"
  )
  imp_aorsf_raw <-
    tibble::enframe(fit_aorsf$importance, name = "predictor", value = "raw")

  imp_aorsf_merge <-
    zv_imp_rf_oblique_res@results |>
    dplyr::full_join(imp_aorsf_raw, by = "predictor") |>
    dplyr::mutate(raw = ifelse(is.na(raw), 0., raw))

  expect_equal(
    imp_aorsf_merge$score,
    imp_aorsf_merge$raw,
    tolerance = 0.1
  )
})

# TODO computations - wrong variable types

test_that("computations - required packages", {
  expect_equal(required_pkgs(score_imp_rf), "filtro")
  expect_equal(required_pkgs(score_imp_rf_conditional), "filtro")
  expect_equal(required_pkgs(score_imp_rf_oblique), "filtro")
})

# TODO Test more after we add validators

test_that("zero-variance predictors", {
  expect_equal(
    filtro:::find_zero_variance_cols(modeldata::leaf_id_flavia),
    "outlying_contour"
  )
  expect_equal(
    filtro:::find_zero_variance_cols(mtcars),
    character(0)
  )
  expect_snapshot(
    filtro:::find_zero_variance_cols(data.frame(y = rep(1, 5), x = 1:5)),
    error = TRUE
  )
  expect_snapshot(
    filtro:::find_zero_variance_cols(data.frame(y = 1:5, x = rep(1, 5))),
    error = TRUE
  )
})
