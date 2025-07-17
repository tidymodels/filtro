helper_perm <- function() {
  modeldata::permeability_qsar |>
    dplyr::select(1:11)
}

helper_perm_factors <- function() {
  helper_perm() |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("chem_fp"), as.factor))
}

