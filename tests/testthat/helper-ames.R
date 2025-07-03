ames_score_obj <- function() {
  skip_if_not_installed("modeldata")
  data <- modeldata::ames |>
    dplyr::select(
      Sale_Price,
      MS_SubClass,
      MS_Zoning,
      Lot_Frontage,
      Lot_Area,
      Street
    )
  outcome <- "Sale_Price"
  score_obj <- score_aov()
  score_res <- get_scores_aov(score_obj, data, outcome)
  score_obj <- score_obj |> attach_score(score_res)
  score_obj
}
