# Adapted from show_best_desirability() in desirability2's tune.R
# TODO Document this
prop_selected <- function(x, ..., prop_terms = 0.99, eval_time = NULL) {
  mtr <- x
  all_vars <- names(mtr)

  res <- desirability2::desirability(..., .use_data = TRUE)

  d_vars <- sort(unique(unlist(res@variables)))
  extra_vars <- setdiff(d_vars, all_vars)
  num_extras <- length(extra_vars)
  if (num_extras > 0) {
    cli::cli_abort(
      "The desirability specification includes {num_extras} variable{?s} that
      {?was/were} not collected: {.val {extra_vars}}."
    )
  }

  # Make individual scores:
  d_nms <- make_col_names(res)
  for (i in seq_along(res@translated)) {
    tmp <- try(rlang::eval_tidy(res@translated[[i]], mtr), silent = TRUE)
    if (inherits(tmp, "try-error")) {
      cli::cli_abort(
        "An error occured when computing a desirability score: {tmp}."
      )
    }
    mtr[[d_nms[i]]] <- tmp
  }

  mtr <-
    mtr |>
    dplyr::mutate(
      .d_overall = d_overall(dplyr::across(dplyr::starts_with(".d_")))
    ) |>
    dplyr::slice_max(.d_overall, prop = prop_terms, with_ties = TRUE)
  mtr
}

make_col_names <- function(x) {
  vars <- purrr::map_chr(x@variables, ~ paste0(.x, collapse = "_"))
  fns <- purrr::map_chr(x@translated, ~ rlang::expr_deparse(.x[[1]]))
  res <- purrr::map2_chr(fns, vars, ~ paste0(".", .x, "_", .y))
  make.names(res, unique = TRUE)
}

# Adapted from show_best_desirability() in desirability2's tune.R
# TODO Document this
num_selected <- function(x, ..., num_terms = 5, eval_time = NULL) {
  mtr <- x
  all_vars <- names(mtr)
  # TODO filter on eval_time

  res <- desirability2::desirability(..., .use_data = TRUE)

  d_vars <- sort(unique(unlist(res@variables)))
  extra_vars <- setdiff(d_vars, all_vars)
  num_extras <- length(extra_vars)
  if (num_extras > 0) {
    cli::cli_abort(
      "The desirability specification includes {num_extras} variable{?s} that
      {?was/were} not collected: {.val {extra_vars}}."
    )
  }

  # Make individual scores:
  d_nms <- make_col_names(res)
  for (i in seq_along(res@translated)) {
    tmp <- try(rlang::eval_tidy(res@translated[[i]], mtr), silent = TRUE)
    if (inherits(tmp, "try-error")) {
      cli::cli_abort(
        "An error occured when computing a desirability score: {tmp}."
      )
    }
    mtr[[d_nms[i]]] <- tmp
  }

  mtr <-
    mtr |>
    dplyr::mutate(
      .d_overall = d_overall(dplyr::across(dplyr::starts_with(".d_")))
    ) |>
    dplyr::slice_max(.d_overall, n = num_terms, with_ties = TRUE)
  mtr
}

cutoff_selected <- function(x, ..., cutoff = 0.01, eval_time = NULL) {
  mtr <- x
  all_vars <- names(mtr)
  # TODO filter on eval_time

  res <- desirability2::desirability(..., .use_data = TRUE)

  d_vars <- sort(unique(unlist(res@variables)))
  extra_vars <- setdiff(d_vars, all_vars)
  num_extras <- length(extra_vars)
  if (num_extras > 0) {
    cli::cli_abort(
      "The desirability specification includes {num_extras} variable{?s} that
      {?was/were} not collected: {.val {extra_vars}}."
    )
  }

  # Make individual scores:
  d_nms <- make_col_names(res)
  for (i in seq_along(res@translated)) {
    tmp <- try(rlang::eval_tidy(res@translated[[i]], mtr), silent = TRUE)
    if (inherits(tmp, "try-error")) {
      cli::cli_abort(
        "An error occured when computing a desirability score: {tmp}."
      )
    }
    mtr[[d_nms[i]]] <- tmp
  }

  mtr <-
    mtr |>
    dplyr::mutate(
      .d_overall = d_overall(dplyr::across(dplyr::starts_with(".d_")))
    ) |>
    dplyr::filter(.d_overall >= cutoff)
  mtr
}

dual_selected <- function(
  x,
  ...,
  prop_terms = 0.99,
  cutoff = NULL,
  eval_time = NULL
) {
  mtr <- x
  all_vars <- names(mtr)

  res <- desirability2::desirability(..., .use_data = TRUE)

  d_vars <- sort(unique(unlist(res@variables)))
  extra_vars <- setdiff(d_vars, all_vars)
  num_extras <- length(extra_vars)
  if (num_extras > 0) {
    cli::cli_abort(
      "The desirability specification includes {num_extras} variable{?s} that
      {?was/were} not collected: {.val {extra_vars}}."
    )
  }

  # Make individual scores:
  d_nms <- make_col_names(res)
  for (i in seq_along(res@translated)) {
    tmp <- try(rlang::eval_tidy(res@translated[[i]], mtr), silent = TRUE)
    if (inherits(tmp, "try-error")) {
      cli::cli_abort(
        "An error occured when computing a desirability score: {tmp}."
      )
    }
    mtr[[d_nms[i]]] <- tmp
  }

  mtr <-
    mtr |>
    dplyr::mutate(
      .d_overall = d_overall(dplyr::across(dplyr::starts_with(".d_")))
    ) |>
    dplyr::slice_max(.d_overall, prop = prop_terms, with_ties = TRUE)

  if (!is.null(cutoff)) {
    mtr <- mtr |> dplyr::filter(.d_overall >= cutoff) # COMMENT Doesn't call cutoff_selected
  }

  mtr
}
