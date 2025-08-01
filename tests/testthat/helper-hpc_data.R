helper_hpc_data <- function() {
  data <- modeldata::hpc_data |>
    dplyr::select(
      class,
      protocol,
      hour
    )
  data
}
