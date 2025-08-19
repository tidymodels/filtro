# Pearson correlation filters - adding missing values and case weights

    Code
      fit(score_cor_pearson, Sale_Price ~ ., data = ames_subset, case_weights = 1)
    Condition
      Error in `fit()`:
      ! There should be as many values in `case_weights` (1) as there are rows in `data` (2930).

---

    Code
      fit(score_cor_pearson, Sale_Price ~ ., data = ames_subset, case_weights = letters)
    Condition
      Error in `fit()`:
      ! `case_weights` should be a numeric or case weight vector, not a character vector

# Spearman correlation filters - adding missing values and case weights

    Code
      fit(score_cor_spearman, Sale_Price ~ ., data = ames_subset, case_weights = 1)
    Condition
      Error in `fit()`:
      ! There should be as many values in `case_weights` (1) as there are rows in `data` (2930).

---

    Code
      fit(score_cor_spearman, Sale_Price ~ ., data = ames_subset, case_weights = letters)
    Condition
      Error in `fit()`:
      ! `case_weights` should be a numeric or case weight vector, not a character vector

