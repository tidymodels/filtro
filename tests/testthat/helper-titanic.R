helper_titanic <- function() {
  data <- titanic::titanic_train |>
    dplyr::mutate(across(c(Survived, Pclass, Sex, Embarked), as.factor)) |>
    dplyr::select(Survived, Pclass, Sex, Age, Fare, Embarked)
  data
}
