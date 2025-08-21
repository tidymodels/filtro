# zero-variance predictors

    Code
      filtro:::find_zero_variance_cols(data.frame(y = rep(1, 5), x = 1:5))
    Condition
      Error:
      ! The outcome column "y" has zero variance and cannot be used.

---

    Code
      filtro:::find_zero_variance_cols(data.frame(y = 1:5, x = rep(1, 5)))
    Condition
      Error:
      ! All of the predictors have zero variance and cannot be used.

