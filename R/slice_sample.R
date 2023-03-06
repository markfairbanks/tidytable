#' @export
#' @rdname slice
slice_sample <- function(.df, n, prop, weight_by = NULL,
                         replace = FALSE, .by = NULL, by = NULL) {
  .df <- .df_as_tidytable(.df)

  if (is_ungrouped(.df)) {
    tt_slice_sample(
      .df, n, prop, {{ weight_by }}, replace, .by = {{ .by }}, by = {{ by }}
    )
  } else {
    .by <- group_vars(.df)
    tt_slice_sample(
      .df, n, prop, {{ weight_by }}, replace, .by = all_of(.by)
    )
  }
}

#' @export
#' @keywords internal
#' @inherit slice
slice_sample. <- function(.df, n, prop, weight_by = NULL,
                          replace = FALSE, .by = NULL, by = NULL) {
  deprecate_dot_fun()
  slice_sample(
    .df, n, prop, {{ weight_by }}, replace, .by = {{ .by }}, by = {{ by }}
  )
}

tt_slice_sample <- function(.df, n, prop, weight_by = NULL,
                            replace = FALSE, .by = NULL, by = NULL) {
  if (missing(n) && missing(prop)) {
    abort("Must supply either `n` or `prop`")
  } else if (missing(prop)) {
    prop <- 1
  } else {
    n <- expr(.N)
  }

  slice(
    .df,
    sample_int(.N, !!n * !!prop, replace, wt = {{ weight_by }}),
    .by = c({{ .by }}, {{ by }})
  )
}

sample_int <- function(n, size, replace = FALSE, wt = NULL) {
  if (replace) {
    sample.int(n, size, prob = wt, replace = TRUE)
  } else {
    sample.int(n, min(size, n), prob = wt)
  }
}
