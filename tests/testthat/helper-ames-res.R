helper_ames_res <- function() {
  utils::data("ames_scores_results", package = "filtro")
  ames_scores_results <- ames_scores_results |>
    dplyr::select(-outcome)
  ames_scores_results
}
