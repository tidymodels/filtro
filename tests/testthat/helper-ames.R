helper_ames <- function() {
  data <- modeldata::ames |>
    dplyr::select(
      Sale_Price,
      MS_SubClass,
      MS_Zoning,
      Lot_Frontage,
      Lot_Area,
      Street
    )
  data
}

helper_ames_v2 <- function() {
  data <- modeldata::ames |>
    dplyr::select(
      Sale_Price,
      MS_SubClass,
      MS_Zoning,
      Lot_Frontage,
      Lot_Area,
      Street,
      Alley,
      Lot_Shape,
      Land_Contour,
      Utilities,
      Lot_Config,
      Land_Slope
    )
  data
}

helper_ames_full <- function() {
  data <- modeldata::ames
}
