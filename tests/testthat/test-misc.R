test_that("transform_abs() works", {
  expect_equal(transform_abs$transform(-1), 1)
  expect_equal(transform_abs$transform(c(-1, 1, 1)), c(1, 1, 1))
})

test_that("transform_neg_log10() works", {
  expect_equal(transform_neg_log10$transform(0.01), -log10(0.01))
  expect_equal(
    transform_neg_log10$transform(c(0.001, 0.01, 0.1)),
    c(-log10(0.001), -log10(0.01), -log10(0.1))
  )
})
