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

test_that("computations - class outcome via ranger", {
  skip_if_not_installed("modeldata")
  cells_subset <- helper_cells()

  score_imp_rf@seed = 42
  cells_imp_rf_res <- score_imp_rf |>
    fit(class ~ ., data = cells_subset)

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

test_that("computations - class outcome via partykit", {
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
    control = partykit::ctree_control(minsplit = 1), # TODO Eventually have user pass in ctree_control()
    ntree = 100,
    mtry = 2,
  )
  imp_partykit_raw <- partykit::varimp(fit_partykit, conditional = TRUE)
  predictors <- setdiff(names(cells_subset), "class")
  imp_partykit <- imp_partykit_raw[predictors] |> unname()
  imp_partykit[is.na(imp_partykit)] <- 0

  expect_equal(cells_imp_rf_conditional_res@results$score, imp_partykit)

  # ----------------------------------------------------------------------------

  expect_equal(cells_imp_rf_conditional_res@range, c(0.0, Inf))
  expect_equal(cells_imp_rf_conditional_res@inclusive, rep(FALSE, 2))
  expect_equal(cells_imp_rf_conditional_res@fallback_value, Inf)
  expect_equal(cells_imp_rf_conditional_res@direction, "maximize")
})

test_that("computations - class outcome via aorsf", {
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

# regression task
ames_subset <- modeldata::ames |>
  dplyr::select(
    Sale_Price,
    MS_SubClass,
    MS_Zoning,
    Lot_Frontage,
    Lot_Area,
    Street
  )
ames_subset <- ames_subset |>
  dplyr::mutate(Sale_Price = log10(Sale_Price))

regression_task <- score_imp_rf
regression_task@mode <- "regression"

set.seed(42)
ames_imp_rf_regression_task_res <-
  regression_task |>
  fit(Sale_Price ~ ., data = ames_subset)
ames_imp_rf_regression_task_res@results

# other configuration
rf_config <- score_imp_rf
# tuning parameters
rf_config@trees <- 100
rf_config@mtry <- 2
rf_config@min_n <- 1
# relevant only for ranger
rf_config@mode <- "regression"
rf_config@seed <- 42

set.seed(42)
ames_imp_rf_config_res <-
  rf_config |>
  fit(Sale_Price ~ ., data = ames_subset)
ames_imp_rf_config_res@results

skip()

test_that("get_score_forest_importance() is working for ranger for classification", {
  skip_if_not_installed("modeldata")

  cells_subset <- helper_cells()
  score_obj <- score_forest_imp(seed = 42)
  score_res <- get_scores_forest_importance(
    score_obj,
    data = cells_subset,
    outcome = "class"
  )

  outcome <- "class"
  y <- cells_subset[[outcome]]
  X <- cells_subset[setdiff(names(cells_subset), outcome)]
  fit <- ranger::ranger(
    y = y,
    x = X,
    num.trees = 10,
    mtry = 2,
    importance = "permutation",
    min.node.size = 1,
    classification = TRUE,
    seed = 42
  )
  exp.res <- unname(fit$variable.importance)

  expect_true(tibble::is_tibble(score_res))

  expect_identical(nrow(score_res), ncol(cells_subset) - 1L)

  expect_named(score_res, c("name", "score", "outcome", "predictor"))

  expect_identical(score_res$score, exp.res)

  expect_equal(unique(score_res$name), "imp_rf")

  expect_equal(unique(score_res$outcome), "class")
})

test_that("get_score_forest_importance() is working for partykit classification", {
  skip_if_not_installed("modeldata")

  cells_subset <- helper_cells()
  score_obj <- score_forest_imp(engine = "partykit")

  set.seed(42)
  score_res <- get_scores_forest_importance(
    score_obj,
    data = cells_subset,
    outcome = "class"
  )

  set.seed(42)
  fit <- partykit::cforest(
    formula = class ~ .,
    data = cells_subset,
    control = partykit::ctree_control(minsplit = 1), # TODO Eventually have user pass in ctree_control()
    ntree = 10,
    mtry = 2,
  )
  imp <- partykit::varimp(fit, conditional = TRUE)
  outcome <- "class"
  predictors <- setdiff(names(cells_subset), outcome)
  exp.imp <- imp[predictors] |> unname()
  exp.imp[is.na(exp.imp)] <- 0

  expect_true(tibble::is_tibble(score_res))

  expect_identical(nrow(score_res), ncol(cells_subset) - 1L)

  expect_named(score_res, c("name", "score", "outcome", "predictor"))

  expect_identical(score_res$score, exp.imp)

  expect_equal(unique(score_res$name), "imp_rf_conditional")

  expect_equal(unique(score_res$outcome), "class")
})

test_that("get_score_forest_importance() is working for aorsf classification", {
  skip_if_not_installed("modeldata")

  cells_subset <- helper_cells()
  score_obj <- score_forest_imp(engine = "aorsf")

  set.seed(42)
  score_res <- get_scores_forest_importance(
    score_obj,
    data = cells_subset,
    outcome = "class"
  )

  set.seed(42)
  fit <- aorsf::orsf(
    formula = class ~ .,
    data = cells_subset,
    n_tree = 10,
    n_retry = 2,
    importance = "permute"
  )
  imp <- fit$importance
  outcome <- "class"
  predictors <- setdiff(names(cells_subset), outcome)
  exp.imp <- imp[predictors] |> unname()
  exp.imp[is.na(exp.imp)] <- 0

  expect_true(tibble::is_tibble(score_res))

  expect_identical(nrow(score_res), ncol(cells_subset) - 1L)

  expect_named(score_res, c("name", "score", "outcome", "predictor"))

  expect_identical(score_res$score, exp.imp)

  expect_equal(unique(score_res$name), "imp_rf_oblique")

  expect_equal(unique(score_res$outcome), "class")
})

test_that("get_score_forest_importance() is working for ranger regression", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()
  score_obj <- score_forest_imp(mode = "regression", seed = 42)
  score_res <- get_scores_forest_importance(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )

  outcome <- "Sale_Price"
  y <- ames_subset[[outcome]]
  X <- ames_subset[setdiff(names(ames_subset), outcome)]
  fit <- ranger::ranger(
    x = X,
    y = y,
    num.trees = 10,
    mtry = 2,
    importance = "permutation",
    min.node.size = 1,
    classification = FALSE,
    seed = 42
  )
  exp.res <- unname(fit$variable.importance)

  expect_true(tibble::is_tibble(score_res))

  expect_identical(nrow(score_res), ncol(ames_subset) - 1L)

  expect_named(score_res, c("name", "score", "outcome", "predictor"))

  expect_identical(score_res$score, exp.res)

  expect_equal(unique(score_res$name), "imp_rf")

  expect_equal(unique(score_res$outcome), "Sale_Price")
})

test_that("get_score_forest_importance() is working for partykit regression", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()
  score_obj <- score_forest_imp(engine = "partykit")
  set.seed(42)
  score_res <- get_scores_forest_importance(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )

  set.seed(42)
  fit <- partykit::cforest(
    formula = Sale_Price ~ .,
    data = ames_subset,
    control = partykit::ctree_control(minsplit = 1), # TODO Eventually have user pass in ctree_control()
    ntree = 10,
    mtry = 2,
  )
  imp <- partykit::varimp(fit, conditional = TRUE)
  outcome <- "Sale_Price"
  predictors <- setdiff(names(ames_subset), outcome)
  exp.imp <- imp[predictors] |> unname()
  exp.imp[is.na(exp.imp)] <- 0

  expect_true(tibble::is_tibble(score_res))

  expect_identical(nrow(score_res), ncol(ames_subset) - 1L)

  expect_named(score_res, c("name", "score", "outcome", "predictor"))

  expect_identical(score_res$score, exp.imp)

  expect_equal(unique(score_res$name), "imp_rf_conditional")

  expect_equal(unique(score_res$outcome), "Sale_Price")
})

test_that("get_score_forest_importance() is working for aorsf regression", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()
  score_obj <- score_forest_imp(engine = "aorsf")
  set.seed(42)
  score_res <- get_scores_forest_importance(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )

  set.seed(42)
  fit <- aorsf::orsf(
    formula = Sale_Price ~ .,
    data = ames_subset,
    n_tree = 10,
    n_retry = 2,
    importance = "permute"
  )
  imp <- fit$importance
  outcome <- "Sale_Price"
  predictors <- setdiff(names(ames_subset), outcome)
  exp.imp <- imp[predictors] |> unname()
  exp.imp[is.na(exp.imp)] <- 0

  expect_true(tibble::is_tibble(score_res))

  expect_identical(nrow(score_res), ncol(ames_subset) - 1L)

  expect_named(score_res, c("name", "score", "outcome", "predictor"))

  expect_identical(score_res$score, exp.imp)

  expect_equal(unique(score_res$name), "imp_rf_oblique")

  expect_equal(unique(score_res$outcome), "Sale_Price")
})

# TODO Test more after we add validators
