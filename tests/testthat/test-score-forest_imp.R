test_that("get_score_forest_importance() is working for ranger for classification", {
  skip_if_not_installed("modeldata")
  data(cells, package = "modeldata") # FYI modeldata::cells() works too
  data <- modeldata::cells |>
    dplyr::select(
      case, # FYI Is not a predictor. Designate whether the row was in the training or test set in the original reference.
      class,
      angle_ch_1,
      area_ch_1,
      avg_inten_ch_1,
      avg_inten_ch_2
    )
  outcome <- "class"
  score_obj <- score_forest_imp()
  score_obj$engine <- "ranger"
  score_obj$trees <- 10
  score_obj$mtry <- 2
  score_obj$min_n <- 1
  score_obj$class <- TRUE # TODO
  set.seed(42)
  res <- get_scores_forest_importance(score_obj, data, outcome)

  set.seed(42)
  fit <- ranger::ranger(
    formula = class ~ .,
    data = data,
    num.trees = 10,
    mtry = 2,
    importance = "permutation",
    min.node.size = 1,
    classification = TRUE,
    seed = 42 # TODO Add this to pass tests. Remove later.
  )
  exp.res <- unname(fit$variable.importance)

  expect_true(tibble::is_tibble(res))

  expect_identical(nrow(res), ncol(data) - 1L)

  expect_named(res, c("name", "score", "outcome", "predictor"))

  expect_identical(res$score, exp.res)

  expect_equal(unique(res$name), "permutation")

  expect_equal(unique(res$outcome), "class")
})

test_that("get_score_forest_importance() is working for ranger regression", {
  skip_if_not_installed("modeldata")
  data(ames, package = "modeldata")
  data <- modeldata::ames |>
    dplyr::select(
      Sale_Price,
      MS_SubClass,
      MS_Zoning,
      Lot_Frontage,
      Lot_Area,
      Street
    )
  outcome <- "Sale_Price"
  score_obj <- score_forest_imp()
  score_obj$engine <- "ranger"
  score_obj$trees <- 10
  score_obj$mtry <- 2
  score_obj$min_n <- 1
  score_obj$class <- FALSE # TODO
  set.seed(42)
  res <- get_scores_forest_importance(score_obj, data, outcome)

  set.seed(42)
  fit <- ranger::ranger(
    formula = Sale_Price ~ .,
    data = data,
    num.trees = 10,
    mtry = 2,
    importance = "permutation",
    min.node.size = 1,
    classification = FALSE,
    seed = 42 # TODO Add this to pass tests. Remove later.
  )
  exp.res <- unname(fit$variable.importance)

  expect_true(tibble::is_tibble(res))

  expect_identical(nrow(res), ncol(data) - 1L)

  expect_named(res, c("name", "score", "outcome", "predictor"))

  expect_identical(res$score, exp.res)

  expect_equal(unique(res$name), "permutation")

  expect_equal(unique(res$outcome), "Sale_Price")
})

test_that("get_score_forest_importance() is working for partykit classification", {
  skip_if_not_installed("modeldata")
  data(cells, package = "modeldata")
  data <- modeldata::cells |>
    dplyr::select(
      case,
      class,
      angle_ch_1,
      area_ch_1,
      avg_inten_ch_1,
      avg_inten_ch_2
    )
  outcome <- "class"
  score_obj <- score_forest_imp()
  score_obj$engine <- "partykit"
  score_obj$trees <- 10
  score_obj$mtry <- 2
  score_obj$min_n <- 1
  set.seed(42)
  res <- get_scores_forest_importance(score_obj, data, outcome)

  set.seed(42)
  fit <- partykit::cforest(
    formula = class ~ .,
    data = data,
    control = partykit::ctree_control(minsplit = 1), # TODO Eventually have user pass in ctree_control()
    ntree = 10,
    mtry = 2,
  )
  imp <- partykit::varimp(fit, conditional = TRUE)
  predictors <- setdiff(names(data), outcome)
  exp.imp <- as.numeric(imp[predictors])
  exp.imp[is.na(exp.imp)] <- 0

  expect_true(tibble::is_tibble(res))

  expect_identical(nrow(res), ncol(data) - 1L)

  expect_named(res, c("name", "score", "outcome", "predictor"))

  expect_identical(res$score, exp.imp)

  expect_equal(unique(res$name), "permutation")

  expect_equal(unique(res$outcome), "class")
})

# TODO Test partykit::cforest regression
# TODO Test aorsf::orsf classification
# TODO Test aorsf::orsf regression
# TODO Test more after we add validators
