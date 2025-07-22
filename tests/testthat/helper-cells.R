helper_cells <- function() {
  data <- modeldata::cells |>
    dplyr::select(
      class,
      angle_ch_1,
      area_ch_1,
      avg_inten_ch_1,
      avg_inten_ch_2,
      avg_inten_ch_3
    )
  data
}
