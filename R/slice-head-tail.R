#' @export
#' @rdname slice
slice_head <- function(.df, n = 5, .by = NULL) {
  UseMethod("slice_head")
}

#' @export
slice_head.tidytable <- function(.df, n = 5, .by = NULL) {
  n <- enquo(n)

  dt_env <- get_dt_env(n)

  n <- prep_expr(n)

  .by <- tidyselect_names(.df, {{ .by }})

  i <- expr(seq.int(min(!!n, .N)))

  dt_expr <- call2_i(.df, i, .by)

  eval_tidy(dt_expr, env = dt_env)
}

#' @export
slice_head.grouped_tt <- function(.df, n = 5, .by = NULL) {
  check_by({{ .by }})
  .by <- group_vars(.df)
  out <- ungroup(.df)
  out <- slice_head(out, {{ n }}, .by = all_of(.by))
  group_by(out, all_of(.by))
}

#' @export
slice_head.data.frame <- function(.df, n = 5, .by = NULL) {
  .df <- as_tidytable(.df)
  slice_head(.df, {{ n }}, .by = {{ .by }})
}

#' @export slice_head.
#' @keywords internal
#' @usage
#' slice_head(.df, n = 5, .by = NULL)
#' @inherit slice_head title description params examples
slice_head. <- function(.df, n = 5, .by = NULL) {
  UseMethod("slice_head.")
}

#' @exportS3Method slice_head. data.frame
slice_head..data.frame <- function(.df, n = 5, .by = NULL) {
  slice_head(.df, {{ n }}, .by = {{ .by }})
}

#' @export
#' @rdname slice
slice_tail <- function(.df, n = 5, .by = NULL) {
  UseMethod("slice_tail")
}

#' @export
slice_tail.tidytable <- function(.df, n = 5, .by = NULL) {
  n <- enquo(n)

  dt_env <- get_dt_env(n)

  n <- prep_expr(n)

  .by <- tidyselect_names(.df, {{ .by }})

  i <- expr(seq.int(.N - min(!!n, .N) + 1, .N))

  dt_expr <- call2_i(.df, i, .by)

  eval_tidy(dt_expr, env = dt_env)
}

#' @export
slice_tail.grouped_tt <- function(.df, n = 5, .by = NULL) {
  check_by({{ .by }})
  .by <- group_vars(.df)
  out <- ungroup(.df)
  out <- slice_tail(out, n, .by = all_of(.by))
  group_by(out, all_of(.by))
}

#' @export
slice_tail.data.frame <- function(.df, n = 5, .by = NULL) {
  .df <- as_tidytable(.df)
  slice_tail(.df, {{ n }}, {{ .by }})
}

#' @export slice_tail.
#' @keywords internal
#' @usage
#' slice_tail(.df, n = 5, .by = NULL)
#' @inherit slice_tail title description params examples
slice_tail. <- function(.df, n = 5, .by = NULL) {
  UseMethod("slice_tail.")
}

#' @exportS3Method slice_head. data.frame
slice_tail..data.frame <- function(.df, n = 5, .by = NULL) {
  slice_tail(.df, {{ n }}, .by = {{ .by }})
}
