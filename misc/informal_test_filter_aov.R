# iris
data(iris)
iris |> str()

# Test lm
outcome <- iris$Sepal.Length
predictor <- iris$Species
fit <- stats::lm(outcome ~ predictor)
#res <- summary(fit)$fstatistic[1] |> as.numeric()
res <- stats::anova(fit)$`F value`[1]
res <- stats::anova(fit)$`Pr(>F)`[1]

# Test flip_if_needed_aov
outcome <- iris$Sepal.Length
predictor <- iris$Species
flipped <- flip_if_needed_aov(x = predictor, y = outcome)
flipped$outcome |> class()
flipped$predictor |> class()

outcome <- iris$Species
predictor <- iris$Sepal.Length
flipped <- flip_if_needed_aov(x = predictor, y = outcome)
flipped$outcome |> class()
flipped$predictor |> class()

# Test get_f_stat
outcome <- iris$Sepal.Length
predictor <- iris$Species
get_f_stat(predictor, outcome)

outcome <- iris$Species
predictor <- iris$Sepal.Length
get_f_stat(predictor, outcome)

# Test get_score
data(iris)
data <- iris
outcome <- "Sepal.Length"
filter_obj <- filter_aov()
tbl_iris <- get_score_aov(filter_obj, data, outcome)
tbl_iris

outcome <- "Species"
filter_obj <- filter_aov()
tbl_iris <- get_score_aov(filter_obj, data, outcome)
tbl_iris

# Test get_score Add score_type = "fstat"
data(iris)
data <- iris
outcome <- "Sepal.Length"
filter_obj <- filter_aov()
#filter_obj$score_type <- "fstat" # Or else Error
tbl_iris <- get_score_aov(filter_obj, data, outcome)
tbl_iris

outcome <- "Species"
filter_obj <- filter_aov()
#filter_obj$score_type <- "fstat" # Or else Error
tbl_iris <- get_score_aov(filter_obj, data, outcome)
tbl_iris

# Test get_score Add score_type = "pval"
data(iris)
data <- iris
outcome <- "Sepal.Length"
filter_obj <- filter_aov()
filter_obj$score_type <- "pval" # Or else Error
tbl_iris <- get_score_aov(filter_obj, data, outcome)
tbl_iris

outcome <- "Species"
filter_obj <- filter_aov()
filter_obj$score_type <- "pval" # Or else Error
tbl_iris <- get_score_aov(filter_obj, data, outcome)
tbl_iris

# cells
data(cells, package = "modeldata")
data <- cells

# Test get_score Add score_type = "fstat"
data <- cells
outcome <- "class"
filter_obj <- filter_aov()
filter_obj$score_type <- "fstat" # Or else Error
tbl_cells <- get_score_aov(filter_obj, data, outcome)
tbl_cells

outcome <- "angle_ch_1"
filter_obj <- filter_aov()
filter_obj$score_type <- "fstat" # Or else Error
tbl_cells <- get_score_aov(filter_obj, data, outcome)
tbl_cells

# Test get_score Add score_type = "pval"
data(iris)
data <- cells
outcome <- "class"
filter_obj <- filter_aov()
filter_obj$score_type <- "pval" # Or else Error
tbl_cells <- get_score_aov(filter_obj, data, outcome)
tbl_cells

outcome <- "angle_ch_1"
filter_obj <- filter_aov()
filter_obj$score_type <- "pval" # Or else Error
tbl_cells <- get_score_aov(filter_obj, data, outcome)
tbl_cells
