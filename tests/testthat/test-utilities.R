test_that("attach_score() is working for roc auc", {
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
  score_obj = score_roc_auc()
  res <- get_scores_roc_auc(score_obj, data, outcome)

  ex.score_obj <- attach_score(score_obj, res)
  ex2.score_obj <- score_obj |> attach_score(res)

  expect_equal(ex.score_obj$res, res)
  expect_equal(ex2.score_obj$res, res)
})

test_that("attach_score() is working for aov", {
  skip_if_not_installed("modeldata")
  data(ames, package = "modeldata")
  data <- tibble::tibble(
    Sale_Price = ames$Sale_Price,
    MS_SubClass = ames$MS_SubClass,
    MS_Zoning = ames$MS_Zoning,
    Lot_Frontage = ames$Lot_Frontage,
    Lot_Area = ames$Lot_Area,
    Street = ames$Street,
  )
  outcome <- "Sale_Price"
  score_obj = score_aov()
  res <- get_scores_aov(score_obj, data, outcome)

  ex.score_obj <- attach_score(score_obj, res)
  ex2.score_obj <- score_obj |> attach_score(res)

  expect_equal(ex.score_obj$res, res)
  expect_equal(ex2.score_obj$res, res)
})

test_that("arrange_score() is working for roc auc", {
  skip_if_not_installed("modeldata")
  data(cells, package = "modeldata")
  data <- tibble::tibble(
    # modeldata::cells |> select()
    case = cells$case,
    class = cells$class,
    angle_ch_1 = cells$angle_ch_1,
    area_ch_1 = cells$area_ch_1,
    avg_inten_ch_1 = cells$avg_inten_ch_1,
    avg_inten_ch_2 = cells$avg_inten_ch_2,
  )
  outcome <- "class"
  score_obj = score_roc_auc()
  res <- get_scores_roc_auc(score_obj, data, outcome)

  score_obj <- score_obj |> attach_score(res) # TODO User have to run this for arrange_score() to work

  score_obj$direction <- "maximize"
  ex.max <- score_obj |> arrange_score()

  score_obj$direction <- "minimize"
  ex.min <- score_obj |> arrange_score()

  score_obj$direction <- "target"
  ex.target <- arrange_score(score_obj, target = 0.760)
  score_obj$direction <- "target"
  ex2.target <- score_obj |> arrange_score(target = 0.760)

  score_obj$direction <- "target"
  ex.target2 <- arrange_score(score_obj, target = 0.502)
  score_obj$direction <- "target"
  ex2.target2 <- score_obj |> arrange_score(target = 0.502)

  expect_equal(ex.max, res |> dplyr::arrange(desc(score)))

  expect_equal(ex.min, res |> dplyr::arrange(score))

  expect_equal(ex.target, res |> dplyr::arrange(abs(score - 0.760)))
  expect_equal(ex2.target, res |> dplyr::arrange(abs(score - 0.760)))

  expect_equal(ex.target2, res |> dplyr::arrange(abs(score - 0.502)))
  expect_equal(ex2.target2, res |> dplyr::arrange(abs(score - 0.502)))
})

test_that("arrange_score() is working for aov", {
  skip_if_not_installed("modeldata")
  data(ames, package = "modeldata")
  data <- tibble::tibble(
    Sale_Price = ames$Sale_Price,
    MS_SubClass = ames$MS_SubClass,
    MS_Zoning = ames$MS_Zoning,
    Lot_Frontage = ames$Lot_Frontage,
    Lot_Area = ames$Lot_Area,
    Street = ames$Street,
  )
  outcome <- "Sale_Price"
  score_obj = score_aov()
  res <- get_scores_aov(score_obj, data, outcome)

  score_obj <- score_obj |> attach_score(res) # TO DO User have to run this for arrange_score() to work

  score_obj$direction <- "maximize"
  ex.max <- score_obj |> arrange_score()

  score_obj$direction <- "minimize"
  ex.min <- score_obj |> arrange_score()

  score_obj$direction <- "target"
  ex.target <- arrange_score(score_obj, target = 63.8)
  score_obj$direction <- "target"
  ex2.target <- score_obj |> arrange_score(target = 63.8)

  score_obj$direction <- "target"
  ex.target2 <- arrange_score(score_obj, target = 10.4)
  score_obj$direction <- "target"
  ex2.target2 <- score_obj |> arrange_score(target = 10.4)

  expect_equal(ex.max, res |> dplyr::arrange(desc(score)))

  expect_equal(ex.min, res |> dplyr::arrange(score))

  expect_equal(ex.target, res |> dplyr::arrange(abs(score - 63.8)))
  expect_equal(ex2.target, res |> dplyr::arrange(abs(score - 63.8)))

  expect_equal(ex.target2, res |> dplyr::arrange(abs(score - 10.4)))
  expect_equal(ex2.target2, res |> dplyr::arrange(abs(score - 10.4)))
})
