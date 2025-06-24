test_that("get_score_forest_importance() is working", {
  skip_if_not_installed("modeldata")
  data(cells, package = "modeldata") # FYI modeldata::cells() works too
  data <- tibble::tibble(
    case = cells$case, # FYI Is not a predictor. Designate whether the row was in the training or test set in the original reference.
    class = cells$class,
    angle_ch_1 = cells$angle_ch_1,
    area_ch_1 = cells$area_ch_1,
    avg_inten_ch_1 = cells$avg_inten_ch_1,
    avg_inten_ch_2 = cells$avg_inten_ch_2,
  )
  outcome <- "class"
  score_obj <- score_forest_imp()
  score_obj$engine <- "ranger"
  score_obj$trees <- 10
  score_obj$mtry <- 2
  score_obj$min_n <- 1
  res <- get_scores_forest_importance(score_obj, data, outcome)

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

# TODO Test aorsf::orsf
# TODO Test regression for ranger::ranger, set classification = FALSE
# TODO Test more after we add validators

test_that("get_score_forest_importance() is working for partykit", {
  skip_if_not_installed("modeldata")
  data(cells, package = "modeldata")
  data <- tibble::tibble(
    case = cells$case,
    class = cells$class,
    angle_ch_1 = cells$angle_ch_1,
    area_ch_1 = cells$area_ch_1,
    avg_inten_ch_1 = cells$avg_inten_ch_1,
    avg_inten_ch_2 = cells$avg_inten_ch_2,
  )
  outcome <- "class"
  score_obj <- score_forest_imp()
  score_obj$engine <- "partykit"
  score_obj$trees <- 10
  score_obj$mtry <- 2
  score_obj$min_n <- 1
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
