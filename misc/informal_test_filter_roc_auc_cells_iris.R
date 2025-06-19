# Two class example
data(cells, package = "modeldata")
cells |> str()

# Multi class example
data(iris)
iris |> str()

# Test get_score
data(iris)
data <- iris
outcome <- "Species"
filter_obj = filter_roc_auc()
tbl_iris <- get_score(filter_obj, data, outcome)
tbl_iris

# Test fit_score
bbb <- fit_score(filter_obj, data, outcome)

# ---------- DRAFT

# Write fit_score_res

fit_score_res <- function(filter_obj) {
  filter_obj$res
}

fit_score_res(bbb)

# Write fit_score_scaling

fit_score_scaling <- function(filter_obj) {
  if (is.null(filter_obj$trans)) {
    trans <- scales::transform_identity()
  } else {
    trans <- filter_obj$trans
  }
  filter_obj$res |>
    mutate(score = trans$transform(score))
}
bbb$trans <- NULL
fit_score_scaling(bbb)
bbb$trans <- scales::transform_log()
fit_score_scaling(bbb)
bbb$trans <- scales::transform_log10()
fit_score_scaling(bbb)
# Customized function in misc.R
bbb$trans <- transform_abs
fit_score_scaling(bbb)
# Customized function in misc.R
bbb$trans <- transform_neg_log10
fit_score_scaling(bbb)

# Write fit_score_arranging

fit_score_arranging <- function(filter_obj, target = 0.993) {
  if (filter_obj$direction == "maximize") {
    filter_obj$res |> arrange(desc(score))
  } else if (filter_obj$direction == "minimize") {
    filter_obj$res |> arrange(score)
  } else if (filter_obj$direction == "target") {
    filter_obj$res |> arrange(abs(score - target))
  }
}

bbb$direction <- "maximize"
fit_score_arranging(bbb)
bbb$direction <- "minimize"
fit_score_arranging(bbb)
bbb$direction <- "target"
fit_score_arranging(bbb)

# Write fit_score_filtering

fit_score_filtering <- function(filter_obj, p = 2, target = 0.993) {
  if (filter_obj$direction == "maximize") {
    filter_obj$res |> arrange(desc(score)) |> slice_head(n = p)
  } else if (filter_obj$direction == "minimize") {
    filter_obj$res |> arrange(score) |> slice_head(n = p)
  } else if (filter_obj$direction == "target") {
    filter_obj$res |> arrange(abs(score - target)) |> slice_head(n = p)
  }
}

bbb$direction <- "maximize"
fit_score_filtering(bbb)
bbb$direction <- "minimize"
fit_score_filtering(bbb)
bbb$direction <- "target"
fit_score_filtering(bbb)

# Write fit_score_filtering_v2

fit_score_filtering_v2 <- function(filter_obj, p = 2, target = 0.993) {
  if (filter_obj$direction == "maximize") {
    filter_obj$res |> slice_max(score, n = p)
  } else if (filter_obj$direction == "minimize") {
    filter_obj$res |> slice_min(score, n = p)
  } else if (filter_obj$direction == "target") {
    filter_obj$res |> arrange(abs(score - target)) |> slice_head(n = p)
  }
}

bbb$direction <- "maximize"
fit_score_filtering_v2(bbb)
bbb$direction <- "minimize"
fit_score_filtering_v2(bbb)
bbb$direction <- "target"
fit_score_filtering_v2(bbb)

# ---------- EXPERIMENT

bbb$res

trans <- scales::transform_identity()
bbb$res |>
  mutate(score = trans$transform(score))

bbb$res

bbb$res |> arrange(score)

bbb$res |> arrange(desc(score))

target <- 0.993
bbb$res |>
  arrange(abs(score - target))

bbb$res |>
  slice_max(score, n = 2)

# OR
bbb$res |>
  arrange(desc(score)) |>
  slice_head(n = 2)

bbb$res |>
  slice_min(score, n = 2)

# OR
bbb$res |>
  arrange(score) |>
  slice_head(n = 2)
