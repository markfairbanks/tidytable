#' @export
#' @rdname slice
slice_sample <- function(.df, n, prop, weight_by = NULL,
                                     replace = FALSE, .by = NULL) {
  slice_sample.(
    .df, n, prop, {{ weight_by }}, replace, .by = {{ .by }}
  )
}

#' @export
#' @keywords internal
#' @inherit slice
slice_sample. <- function(.df, n, prop, weight_by = NULL,
                         replace = FALSE, .by = NULL) {
  UseMethod("slice_sample.")
}

#' @export
slice_sample..tidytable <- function(.df, n, prop, weight_by = NULL,
                                   replace = FALSE, .by = NULL) {
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
    .by = {{ .by }}
  )
}

#' @export
slice_sample..grouped_tt <- function(.df, n, prop, weight_by = NULL,
                                    replace = FALSE, .by = NULL) {
  check_by({{ .by }})
  .by <- group_vars(.df)
  out <- ungroup(.df)
  out <- slice_sample(
    out, n, prop, {{ weight_by }}, replace, .by = all_of(.by)
  )
  group_by(out, all_of(.by))
}

#' @export
slice_sample..data.frame <- function(.df, n, prop, weight_by = NULL,
                                    replace = FALSE, .by = NULL) {
  .df <- as_tidytable(.df)
  slice_sample(
    .df, n, prop, {{ weight_by }}, replace, .by = {{ .by }}
  )
}

sample_int <- function(n, size, replace = FALSE, wt = NULL) {
  if (replace) {
    sample.int(n, size, prob = wt, replace = TRUE)
  } else {
    sample.int(n, min(size, n), prob = wt)
  }
}
