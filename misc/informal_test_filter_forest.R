# iris
data(iris)
iris |> str()
data <- iris

# test ranger classfication
fit <- ranger::ranger(
  formula = Species ~ .,
  data = iris,
  num.trees = 10,
  mtry = 2,
  importance = "permutation",
  min.node.size = NULL,
  classification = TRUE
)
res <- fit$variable.importance
res

# test ranger regression
fit <- ranger::ranger(
  formula = Sepal.Length ~ .,
  data = iris,
  num.trees = 10,
  mtry = 2,
  importance = "permutation",
  min.node.size = NULL,
  classification = FALSE
)
res <- fit$variable.importance
res

# test cforest classfication. Do not use ?. Check documentation on line.
fit <- partykit::cforest(
  formula = Species ~ .,
  data = iris,
  control = ctree_control(
    minsplit = 1L,
    minbucket = 1L,
  ),
  ntree = 10,
  mtry = 2
)
res <- varimp(fit) # permutation
res
res <- varimp(fit, conditional = TRUE) # conditional permutation
res

# test cforest regression
fit <- partykit::cforest(
  formula = Sepal.Length ~ .,
  data = iris,
  control = ctree_control(
    minsplit = 1L,
    minbucket = 1L,
  ),
  ntree = 10,
  mtry = 2
)
res <- varimp(fit) # permutation
res
res <- varimp(fit, conditional = TRUE) # conditional permutation
res

# test orsf classfication
fit <- aorsf::orsf(
  formula = Species ~ .,
  data = iris,
  n_tree = 10,
  n_retry = 2,
  importance = "permute"
)
res <- fit$importance # orsf_vi_permute(fit)
res

# test orsf regression
fit <- aorsf::orsf(
  formula = Sepal.Length ~ .,
  data = iris,
  n_tree = 10,
  n_retry = 2,
  importance = "permute"
)
res <- fit$importance # orsf_vi_permute(fit)
res

# Test get_score_importance on ranger
data(iris)
data <- iris
outcome <- "Species"
score_obj <- score_forest()
tbl_iris <- get_score_importance(score_obj, data, outcome)
tbl_iris

data(iris)
data <- iris
outcome <- "Species"
score_obj <- score_forest()
score_obj$engine <- "ranger" # Add engine
tbl_iris <- get_score_importance(score_obj, data, outcome)
tbl_iris

# Test get_score_importance on partykit
data(iris)
data <- iris
outcome <- "Species"
score_obj <- score_forest()
score_obj$engine <- "partykit"
tbl_iris <- get_score_importance(score_obj, data, outcome)
tbl_iris

# Test get_score_importance on partykit
data(iris)
data <- iris
outcome <- "Species"
score_obj <- score_forest()
score_obj$engine <- "aorsf"
tbl_iris <- get_score_importance(score_obj, data, outcome)
tbl_iris

# Test
data(iris)
data <- iris
outcome <- "Species"
score_obj <- score_forest()
score_obj$engine <- "ranger"
score_obj$trees <- 10
score_obj$mtry <- 2
score_obj$min_n <- 1
tbl_iris <- get_score_importance(score_obj, data, outcome)
tbl_iris

outcome <- "Species"
score_obj <- score_forest()
score_obj$engine <- "partykit"
score_obj$trees <- 10
score_obj$mtry <- 2
score_obj$min_n <- 1
tbl_iris <- get_score_importance(score_obj, data, outcome)
tbl_iris

outcome <- "Species"
score_obj <- score_forest()
score_obj$engine <- "aorsf"
score_obj$trees <- 10
score_obj$mtry <- 2
score_obj$min_n <- 1
tbl_iris <- get_score_importance(score_obj, data, outcome)
tbl_iris
