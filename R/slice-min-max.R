#' @export
#' @rdname slice
slice_max <- function(.df, order_by, n = 1, ..., with_ties = TRUE,
                      .by = NULL, by = NULL) {
  check_required(order_by)
  UseMethod("slice_max")
}

#' @export
slice_max.tidytable <- function(.df, order_by, n = 1, ..., with_ties = TRUE,
                                 .by = NULL, by = NULL) {
  if (is_true(with_ties)) {
    .df %>%
      filter(
        frank({{ order_by }}, ties.method = "max") > (.N - .env$n),
        .by = c({{ .by }}, {{ by }})
      ) %>%
      arrange(-{{ order_by }})
  } else {
    .df %>%
      arrange(-{{ order_by }}) %>%
      slice_head(n, .by = c({{ .by }}, {{ by }}))
  }
}

#' @export
slice_max.grouped_tt <- function(.df, order_by, n = 1, ..., with_ties = TRUE,
                                 .by = NULL, by = NULL) {
  .by <- group_vars(.df)
  out <- ungroup(.df)
  out <- slice_max(
    out, {{ order_by }}, {{ n }}, with_ties = with_ties, .by = all_of(.by)
  )
  group_by(out, all_of(.by))
}

#' @export
slice_max.data.frame <- function(.df, order_by, n = 1, ..., with_ties = TRUE,
                                 .by = NULL, by = NULL) {
  .df <- as_tidytable(.df)
  slice_max(
    .df, {{ order_by }}, {{ n }}, with_ties = with_ties,
    .by = {{ .by }}, by = {{ by }}
  )
}

#' @export
#' @rdname slice
slice_min <- function(.df, order_by, n = 1, ..., with_ties = TRUE,
                      .by = NULL, by = NULL) {
  check_required(order_by)
  UseMethod("slice_min")
}

#' @export
slice_min.tidytable <- function(.df, order_by, n = 1, ..., with_ties = TRUE,
                                .by = NULL, by = NULL) {
  if (is_true(with_ties)) {
    .df %>%
      filter(
        frank({{ order_by }}, ties.method = "min") <= .env$n,
        .by = c({{ .by }}, {{ by }})
      ) %>%
      arrange({{ order_by }})
  } else {
    .df %>%
      arrange({{ order_by }}) %>%
      slice_head(n, .by = c({{ .by }}, {{ by }}))
  }
}

#' @export
slice_min.grouped_tt <- function(.df, order_by, n = 1, ..., with_ties = TRUE,
                                 .by = NULL, by = NULL) {
  .by <- group_vars(.df)
  out <- ungroup(.df)
  out <- slice_min(
    out, {{ order_by }}, {{ n }}, with_ties = with_ties, .by = any_of(.by)
  )
  group_by(out, any_of(.by))
}

#' @export
slice_min.data.frame <- function(.df, order_by, n = 1, ..., with_ties = TRUE,
                                 .by = NULL, by = NULL) {
  .df <- as_tidytable(.df)
  slice_min(
    .df, {{ order_by }}, {{ n }}, with_ties = with_ties,
    .by = {{ .by }}, by = {{ by }}
  )
}

