# titanic
library(titanic)
titanic_train |> str()
titanic_train$Sex <- titanic_train$Sex |> as.factor()

# Test chisq.test, fisher.test
outcome <- titanic_train$Survived
predictor <- titanic_train$Sex
tab <- table(predictor, outcome)
stats::chisq.test(tab)$p.value
stats::fisher.test(tab)$p.value

# Test get_chisq
outcome <- titanic_train$Survived
predictor <- titanic_train$Sex
get_chisq(predictor, outcome)
outcome <- titanic_train$Sex
predictor <- titanic_train$Survived
get_chisq(predictor, outcome)

# Test get_fisher
outcome <- titanic_train$Survived
predictor <- titanic_train$Sex
get_fisher(predictor, outcome)
outcome <- titanic_train$Sex
predictor <- titanic_train$Survived
get_fisher(predictor, outcome)

# Test get_score_cross_tab
library(titanic)
titanic_train$Survived <- titanic_train$Survived |> as.factor()
titanic_train$Pclass <- titanic_train$Pclass |> as.factor()
titanic_train$Sex <- titanic_train$Sex |> as.factor()
titanic_train$Embarked <- titanic_train$Embarked |> as.factor()
# Subset for now because there are other data types such as chr
data <- tibble(
  Survived = titanic_train$Survived,
  Pclass = titanic_train$Pclass,
  Sex = titanic_train$Sex,
  Age = titanic_train$Age,
  Fare = titanic_train$Fare,
  Embarked = titanic_train$Embarked
)
outcome <- "Survived"
filter_obj <- filter_cross_tab()
tbl_titanic <- get_score_cross_tab(filter_obj, data, outcome)
tbl_titanic

outcome <- "Survived"
filter_obj <- filter_cross_tab()
filter_obj$score_type <- "fisher"
tbl_titanic <- get_score_cross_tab(filter_obj, data, outcome)
tbl_titanic
