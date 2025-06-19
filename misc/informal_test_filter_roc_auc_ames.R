# Two class example
data(ames, package = "modeldata")
ames |> str()
outcome <- ames$Sale_Price
predictor <- ames$Street

roc <- pROC::roc(predictor, outcome, direction = "auto")
res <- pROC::auc(roc)

# Multi class example
outcome <- ames$Sale_Price
predictor <- ames$MS_SubClass
roc <- pROC::roc(predictor, outcome, direction = "auto")
res <- pROC::auc(roc)

outcome <- ames$Sale_Price
predictor <- ames$MS_SubClass
roc <- pROC::multiclass.roc(predictor, outcome, direction = "auto")
res <- pROC::auc(roc)

roc <- pROC::multiclass.roc(ames$MS_Zoning, outcome, direction = "auto")
res <- pROC::auc(roc)

# Test get_roc_auc
outcome <- ames$Sale_Price
predictor <- ames$Street
get_roc_auc(predictor, outcome)
get_roc_auc(ames$Central_Air, outcome)

# Test get_all_roc_auc
data(ames, package = "modeldata")
data <- ames
outcome <- "Sale_Price"
filter_obj = filter_roc_auc()
tbl <- get_score(filter_obj, data, outcome)
tbl
