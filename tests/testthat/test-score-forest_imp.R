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

test_that("computations - classification task via ranger", {
  skip_if_not_installed("modeldata")

  cells_subset <- helper_cells()

  score_imp_rf@seed <- 42
  cells_imp_rf_res <- score_imp_rf |>
    fit(
      class ~ .,
      seed = 42,
      data = cells_subset
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
    min.node.size = 1,
    classification = TRUE,
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

  regression_task <- score_imp_rf
  regression_task@mode <- "regression"
  set.seed(42)
  ames_imp_rf_regression_task_res <-
    regression_task |>
    fit(
      Sale_Price ~ .,
      data = ames_subset,
      seed = 42
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
    min.node.size = 1,
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

  # ----------------------------------------------------------------------------

  expect_equal(ames_imp_rf_regression_task_res@mode, "regression")
})

test_that("computations - regression task via ranger vary trees, mtry, min_n", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()
  ames_subset <- ames_subset |>
    dplyr::mutate(Sale_Price = log10(Sale_Price))

  regression_task <- score_imp_rf
  regression_task@mode <- "regression"
  set.seed(42)
  ames_imp_rf_regression_task_res <-
    regression_task |>
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

  # ----------------------------------------------------------------------------

  expect_equal(ames_imp_rf_regression_task_res@mode, "regression")
})

test_that("computations - regression task via ranger - adding missing values and case weights", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()
  ames_subset <- ames_subset |>
    dplyr::mutate(Sale_Price = log10(Sale_Price))

  # ----------------------------------------------------------------------------
  # missing values

  ames_missing <- ames_subset
  ames_missing$Sale_Price[1] <- NA_real_
  ames_missing$Lot_Frontage[2] <- NA_real_

  regression_task <- score_imp_rf
  regression_task@mode <- "regression"
  set.seed(42)
  ames_missing_imp_rf_res <-
    regression_task |>
    fit(
      Sale_Price ~ .,
      data = ames_missing,
      seed = 42
    )

  # ----------------------------------------------------------------------------

  y <- ames_missing[["Sale_Price"]]
  X <- ames_missing[setdiff(names(ames_missing), "Sale_Price")]

  compete_obs <- stats::complete.cases(X, y)
  y <- y[compete_obs]
  X <- X[compete_obs, , drop = FALSE]

  fit_ranger <- ranger::ranger(
    y = y,
    x = X,
    num.trees = 100,
    mtry = 2,
    importance = "permutation",
    min.node.size = 1,
    classification = FALSE,
    seed = 42
  )
  imp_ranger <- (fit_ranger$variable.importance) |> unname()

  expect_equal(ames_missing_imp_rf_res@results$score, imp_ranger)

  # ----------------------------------------------------------------------------
  # case weights

  two_weights <- c(rep(1, 10), rep(0, nrow(ames_subset) - 10))

  regression_task <- score_imp_rf
  regression_task@mode <- "regression"
  set.seed(42)
  ames_weights_imp_rf_res <-
    regression_task |>
    fit(
      Sale_Price ~ .,
      data = ames_subset,
      seed = 42,
      case_weights = two_weights
    )

  # ----------------------------------------------------------------------------

  y <- ames_subset[["Sale_Price"]]
  X <- ames_subset[setdiff(names(ames_missing), "Sale_Price")]

  fit_ranger <- ranger::ranger(
    y = y,
    x = X,
    num.trees = 100,
    mtry = 2,
    importance = "permutation",
    min.node.size = 1,
    classification = FALSE,
    seed = 42,
    case.weights = two_weights
  )
  imp_ranger <- (fit_ranger$variable.importance) |> unname()

  expect_equal(ames_weights_imp_rf_res@results$score, imp_ranger)
})

test_that("computations - classification task via partykit", {
  skip_if_not_installed("modeldata")

  cells_subset <- helper_cells()

  set.seed(42)
  cells_imp_rf_conditional_res <- score_imp_rf_conditional |>
    fit(class ~ ., data = cells_subset)

  # ----------------------------------------------------------------------------

  set.seed(42)
  fit_partykit <- partykit::cforest(
    formula = class ~ .,
    data = cells_subset,
    ntree = 100,
    mtry = 2,
    control = partykit::ctree_control(minsplit = 1) # TODO Eventually have user pass in ctree_control()
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

  ames_subset <- helper_ames()
  ames_subset <- ames_subset |>
    dplyr::mutate(Sale_Price = log10(Sale_Price))

  set.seed(42)
  ames_missing_imp_rf_conditional_res <- score_imp_rf_conditional |>
    fit(Sale_Price ~ ., data = ames_subset)

  # ----------------------------------------------------------------------------

  set.seed(42)
  fit_partykit <- partykit::cforest(
    formula = Sale_Price ~ .,
    data = ames_subset,
    ntree = 100,
    mtry = 2,
    control = partykit::ctree_control(minsplit = 1) # TODO Eventually have user pass in ctree_control()
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

test_that("computations - regression task via partykit - adding missing values and case weights", {
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
  ames_missing_imp_rf_conditional_res <- score_imp_rf_conditional |>
    fit(Sale_Price ~ ., data = ames_missing)

  # ----------------------------------------------------------------------------

  ames_missing <- ames_missing[stats::complete.cases(ames_missing), ]

  set.seed(42)
  fit_partykit <- partykit::cforest(
    formula = Sale_Price ~ .,
    data = ames_missing,
    ntree = 100,
    mtry = 2,
    control = partykit::ctree_control(minsplit = 1) # TODO Eventually have user pass in ctree_control()
  )
  imp_partykit_raw <- partykit::varimp(fit_partykit, conditional = TRUE)
  predictors <- setdiff(names(ames_subset), "Sale_Price")
  imp_partykit <- imp_partykit_raw[predictors] |> unname()
  imp_partykit[is.na(imp_partykit)] <- 0

  expect_equal(
    ames_missing_imp_rf_conditional_res@results$score,
    imp_partykit,
    tolerance = 0.1
  )

  # ----------------------------------------------------------------------------
  # case weights
  two_weights <- c(rep(1, 2000), rep(0, nrow(ames_subset) - 2000)) # TODO Throw error if the probability is too low

  set.seed(42)
  ames_weights_imp_rf_conditional_res <- score_imp_rf_conditional |>
    fit(Sale_Price ~ ., data = ames_subset, case_weights = two_weights)

  # ----------------------------------------------------------------------------

  set.seed(42)
  fit_partykit <- partykit::cforest(
    formula = Sale_Price ~ .,
    data = ames_subset,
    ntree = 100,
    mtry = 2,
    control = partykit::ctree_control(minsplit = 1), # TODO Eventually have user pass in ctree_control()
    weights = two_weights
  )
  imp_partykit_raw <- partykit::varimp(fit_partykit, conditional = TRUE)
  predictors <- setdiff(names(ames_subset), "Sale_Price")
  imp_partykit <- imp_partykit_raw[predictors] |> unname()
  imp_partykit[is.na(imp_partykit)] <- 0

  expect_equal(
    ames_weights_imp_rf_conditional_res@results$score,
    imp_partykit,
    tolerance = 0.1
  )
})

test_that("computations - classification task via aorsf", {
  skip_if_not_installed("modeldata")

  cells_subset <- helper_cells()

  set.seed(42)
  cells_imp_rf_oblique_res <- score_imp_rf_oblique |>
    fit(class ~ ., data = cells_subset)

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
    fit(Sale_Price ~ ., data = ames_subset)

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
    n_tree = 100,
    n_retry = 2,
    importance = "permute"
  )
  imp_raw_aorsf <- fit_aorsf$importance
  predictors <- setdiff(names(ames_subset), "Sale_Price")
  imp_aorsf <- imp_raw_aorsf[predictors] |> unname()
  imp_aorsf[is.na(imp_aorsf)] <- 0

  expect_equal(ames_missing_imp_rf_oblique_res@results$score, imp_aorsf)

  # ----------------------------------------------------------------------------
  # case weights
  two_weights <- c(rep(1, 10), rep(0, nrow(ames_subset) - 10))

  set.seed(42)
  ames_weights_imp_rf_oblique_res <- score_imp_rf_oblique |>
    fit(Sale_Price ~ ., data = ames_subset, case_weights = two_weights)

  # ----------------------------------------------------------------------------

  set.seed(42)
  fit_aorsf <- aorsf::orsf(
    formula = Sale_Price ~ .,
    data = ames_subset,
    n_tree = 100,
    n_retry = 2,
    importance = "permute",
    weights = two_weights
  )
  imp_raw_aorsf <- fit_aorsf$importance
  predictors <- setdiff(names(ames_subset), "Sale_Price")
  imp_aorsf <- imp_raw_aorsf[predictors] |> unname()
  imp_aorsf[is.na(imp_aorsf)] <- 0

  expect_equal(ames_weights_imp_rf_oblique_res@results$score, imp_aorsf)
})

# TODO computations - wrong variable types

test_that("computations - required packages", {
  expect_equal(required_pkgs(score_imp_rf), "filtro")
  expect_equal(required_pkgs(score_imp_rf_conditional), "filtro")
  expect_equal(required_pkgs(score_imp_rf_oblique), "filtro")
})

# TODO Test more after we add validators
