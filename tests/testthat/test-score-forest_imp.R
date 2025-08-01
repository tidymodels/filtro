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
  before_3 <- list(trees = 5, regularization.factor = 1/2, min.node.size = 1)
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
  cells_subset <- helper_cells() |> dplyr::slice(1:50)

  set.seed(42)
  cells_imp_rf_conditional_res <- score_imp_rf_conditional |>
    fit(class ~ ., data = cells_subset, trees = 3)

  # ----------------------------------------------------------------------------

  set.seed(42)
  fit_partykit <- partykit::cforest(
    formula = class ~ .,
    data = cells_subset,
    ntree = 3
  )
  imp_partykit_raw <- partykit::varimp(fit_partykit, conditional = TRUE)
  predictors <- setdiff(names(cells_subset), "class")
  imp_partykit <- imp_partykit_raw[predictors] |> unname()
  imp_partykit[is.na(imp_partykit)] <- 0

  expect_equal(
    cells_imp_rf_conditional_res@results$score,
    imp_partykit,
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
    dplyr::slice_sample(n = 10, by = Street) |>
    dplyr::select(Sale_Price, Lot_Area, Street)

  ames_subset <- ames_subset |>
    dplyr::mutate(Sale_Price = log10(Sale_Price))

  set.seed(42)
  ames_imp_rf_conditional_res <- score_imp_rf_conditional |>
    fit(Sale_Price ~ ., data = ames_subset, trees = 3)

  # ----------------------------------------------------------------------------

  set.seed(42)
  fit_partykit <- partykit::cforest(
    formula = Sale_Price ~ .,
    data = ames_subset,
    ntree = 3
  )
  imp_partykit_raw <- partykit::varimp(fit_partykit, conditional = TRUE)
  predictors <- setdiff(names(ames_subset), "Sale_Price")
  imp_partykit <- imp_partykit_raw[predictors] |> unname()
  imp_partykit[is.na(imp_partykit)] <- 0

  expect_equal(
    ames_imp_rf_conditional_res@results$score,
    imp_partykit,
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
  imp_raw_aorsf <- fit_aorsf$importance
  predictors <- setdiff(names(cells_subset), "class")
  imp_aorsf <- imp_raw_aorsf[predictors] |> unname()
  imp_aorsf[is.na(imp_aorsf)] <- 0

  expect_equal(cells_imp_rf_oblique_res@results$score, imp_aorsf)

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
  imp_raw_aorsf <- fit_aorsf$importance
  predictors <- setdiff(names(ames_subset), "Sale_Price")
  imp_aorsf <- imp_raw_aorsf[predictors] |> unname()
  imp_aorsf[is.na(imp_aorsf)] <- 0

  expect_equal(ames_imp_rf_oblique_res@results$score, imp_aorsf)

  # ----------------------------------------------------------------------------

  expect_equal(ames_imp_rf_oblique_res@range, c(0.0, Inf))
  expect_equal(ames_imp_rf_oblique_res@inclusive, rep(FALSE, 2))
  expect_equal(ames_imp_rf_oblique_res@fallback_value, Inf)
  expect_equal(ames_imp_rf_oblique_res@direction, "maximize")
})

# TODO computations - wrong variable types

test_that("computations - required packages", {
  expect_equal(required_pkgs(score_imp_rf), "filtro")
  expect_equal(required_pkgs(score_imp_rf_conditional), "filtro")
  expect_equal(required_pkgs(score_imp_rf_oblique), "filtro")
})

# TODO Test more after we add validators
