#' @export
#' @rdname slice
slice_max <- function(.df, order_by, n = 1, ..., with_ties = TRUE,
                      .by = NULL, by = NULL) {
  check_required(order_by)

  .df <- .df_as_tidytable(.df)

  if (is_ungrouped(.df)) {
    tt_slice_max(
      .df, {{ order_by }}, {{ n }}, with_ties = with_ties,
      .by = {{ .by }}, by = {{ by }}
    )
  } else {
    .by <- group_vars(.df)
    tt_slice_max(
      .df, {{ order_by }}, {{ n }}, with_ties = with_ties, .by = all_of(.by)
    )
  }
}

#' @export
#' @keywords internal
#' @inherit slice
slice_max. <- function(.df, order_by, n = 1, ..., with_ties = TRUE,
                       .by = NULL, by = NULL) {
  check_required(order_by)
  deprecate_dot_fun()
  slice_max(
    .df, {{ order_by }}, {{ n }}, with_ties = with_ties,
    .by = {{ .by }}, by = {{ by }}
  )
}

tt_slice_max <- function(.df, order_by, n = 1, ..., with_ties = TRUE,
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
#' @rdname slice
slice_min <- function(.df, order_by, n = 1, ..., with_ties = TRUE,
                      .by = NULL, by = NULL) {
  check_required(order_by)

  .df <- .df_as_tidytable(.df)

  if (is_ungrouped(.df)) {
    tt_slice_min(
      .df, {{ order_by }}, {{ n }}, with_ties = with_ties,
      .by = {{ .by }}, by = {{ by }}
    )
  } else {
    .by <- group_vars(.df)
    tt_slice_min(
      .df, {{ order_by }}, {{ n }}, with_ties = with_ties, .by = all_of(.by)
    )
  }
}

#' @export
#' @keywords internal
#' @inherit slice
slice_min. <- function(.df, order_by, n = 1, ..., with_ties = TRUE,
                       .by = NULL, by = NULL) {
  check_required(order_by)
  deprecate_dot_fun()
  slice_min(
    .df, {{ order_by }}, {{ n }}, with_ties = with_ties,
    .by = {{ .by }}, by = {{ by }}
  )
}

tt_slice_min <- function(.df, order_by, n = 1, ..., with_ties = TRUE,
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


