test_that("get_score_forest_importance() is working for ranger for classification", {
  skip_if_not_installed("modeldata")

  cells_subset <- helper_cells()
  score_obj <- score_forest_imp()

  set.seed(42)
  score_res <- get_scores_forest_importance(
    score_obj,
    data = cells_subset,
    outcome = "class"
  )

  outcome <- "class"
  y <- cells_subset[[outcome]]
  X <- cells_subset[setdiff(names(cells_subset), outcome)]
  set.seed(42)
  fit <- ranger::ranger(
    y = y,
    x = X,
    num.trees = 10,
    mtry = 2,
    importance = "permutation",
    min.node.size = 1,
    classification = TRUE,
    seed = 42 # TODO Add this to pass tests. Remove later. set.seed(42) alone does not seem to work.
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
  exp.imp <- as.numeric(imp[predictors])
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
  exp.imp <- as.numeric(imp[predictors])
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
  score_obj <- score_forest_imp(is_reg = TRUE)
  set.seed(42)
  score_res <- get_scores_forest_importance(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )

  outcome <- "Sale_Price"
  y <- ames_subset[[outcome]]
  X <- ames_subset[setdiff(names(ames_subset), outcome)]
  set.seed(42)
  fit <- ranger::ranger(
    x = X,
    y = y,
    num.trees = 10,
    mtry = 2,
    importance = "permutation",
    min.node.size = 1,
    classification = FALSE,
    seed = 42 # TODO Add this to pass tests. Remove later. set.seed(42) alone does not seem to work.
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
  score_obj <- score_forest_imp(engine = "partykit", is_reg = TRUE)
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
  exp.imp <- as.numeric(imp[predictors])
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
  score_obj <- score_forest_imp(engine = "aorsf", is_reg = TRUE)
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
  exp.imp <- as.numeric(imp[predictors])
  exp.imp[is.na(exp.imp)] <- 0

  expect_true(tibble::is_tibble(score_res))

  expect_identical(nrow(score_res), ncol(ames_subset) - 1L)

  expect_named(score_res, c("name", "score", "outcome", "predictor"))

  expect_identical(score_res$score, exp.imp)

  expect_equal(unique(score_res$name), "imp_rf_oblique")

  expect_equal(unique(score_res$outcome), "Sale_Price")
})

# TODO Test more after we add validators
