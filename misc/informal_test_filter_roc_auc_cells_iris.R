# Two class example
data(cells, package = "modeldata")
cells |> str()
outcome <- cells$class
predictor <- cells$avg_inten_ch_1

roc <- pROC::roc(outcome, predictor, direction = "auto")
res <- pROC::auc(roc)

# Multi class example
data(iris)
iris |> str()
outcome <- iris$Species
predictor <- iris$Sepal.Length

# Throw warning which is good!
roc <- pROC::roc(outcome, predictor, direction = "auto")
res <- pROC::auc(roc) |> as.numeric()

roc <- pROC::multiclass.roc(outcome, predictor, direction = "auto")
res <- pROC::auc(roc) |> as.numeric()

# Test get_roc_auc
# Two class
y <- cells$class
x <- cells$avg_inten_ch_1
get_roc_auc(x, y)

get_roc_auc(cells$case, y)
get_roc_auc(cells$angle_ch_1, y)
get_roc_auc(cells$area_ch_1, y)
get_roc_auc(cells$avg_inten_ch_1, y)
get_roc_auc(cells$avg_inten_ch_2, y)
get_roc_auc(cells$avg_inten_ch_3, y)
get_roc_auc(cells$avg_inten_ch_4, y)

# y <- cells$avg_inten_ch_1 # To do
# x <- cells$class
# get_roc_auc(x, y)
# get_roc_auc(y, y)
# get_roc_auc(x, x)

# Test get_all_roc_auc
# Two class
data <- cells
outcome <- "class"
filter_obj = filter_roc_auc()
tbl <- get_score(filter_obj, data, outcome)
tbl

# Test calc_score
# Two class
data <- cells
outcome <- "class"
bbb <- calc_score(filter_obj, data, outcome)
bbb
bbb$res
class(bbb)
class(bbb$res)
