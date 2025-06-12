test_that("mean_difference works with positive numbers", {
  x <- c(1, 2, 3)
  y <- c(4, 5, 6)
  expect_equal(mean_difference(x, y), 3)
})

test_that("mean_difference works with negative numbers", {
  x <- c(-10, -20)
  y <- c(-30, -40)
  expect_equal(mean_difference(x, y), 20)
})
