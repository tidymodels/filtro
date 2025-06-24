# iris
data(iris)
iris |> str()
data <- iris

# Test cor
outcome <- data$Sepal.Length
predictor <- data$Sepal.Width
stats::cor(outcome, predictor, method = "pearson")
stats::cor(outcome, predictor, method = "spearman")

# Test get_cor
outcome <- data$Sepal.Length
predictor <- data$Sepal.Width
get_cor(outcome, predictor)

# Test get_score_cor
data(iris)
data <- iris
outcome <- "Sepal.Length"
filter_obj <- filter_cor()
filter_obj$score_type <- "pearson"
tbl_iris <- get_score_cor(filter_obj, data, outcome)
tbl_iris

filter_obj <- filter_cor()
filter_obj$score_type <- "spearman"
tbl_iris <- get_score_cor(filter_obj, data, outcome)
tbl_iris

outcome <- "Species"
filter_obj <- filter_cor()
filter_obj$score_type <- "pearson"
tbl_iris <- get_score_cor(filter_obj, data, outcome)
tbl_iris

filter_obj <- filter_cor()
filter_obj$score_type <- "spearman"
tbl_iris <- get_score_cor(filter_obj, data, outcome)
tbl_iris
