#' @export
#' @rdname slice
slice_head <- function(.df, n = 5, ..., .by = NULL, by = NULL) {
  UseMethod("slice_head")
}

#' @export
slice_head.tidytable <- function(.df, n = 5, ..., .by = NULL, by = NULL) {
  n <- enquo(n)

  dt_env <- get_dt_env(n)

  n <- prep_expr(n)

  .by <- tidyselect_names(.df, c({{ .by }}, {{ by }}))

  i <- expr(seq_len(min(!!n, .N)))

  dt_expr <- call2_i(.df, i, .by)

  eval_tidy(dt_expr, env = dt_env)
}

#' @export
slice_head.grouped_tt <- function(.df, n = 5, ..., .by = NULL, by = NULL) {
  .by <- group_vars(.df)
  out <- ungroup(.df)
  out <- slice_head(out, {{ n }}, .by = any_of(.by))
  group_by(out, any_of(.by))
}

#' @export
slice_head.data.frame <- function(.df, n = 5, ..., .by = NULL, by = NULL) {
  .df <- as_tidytable(.df)
  slice_head(.df, {{ n }}, .by = {{ .by }}, by = {{ by }})
}

#' @export
#' @rdname slice
slice_tail <- function(.df, n = 5, ..., .by = NULL, by = NULL) {
  UseMethod("slice_tail")
}

#' @export
slice_tail.tidytable <- function(.df, n = 5, ..., .by = NULL, by = NULL) {
  n <- enquo(n)

  dt_env <- get_dt_env(n)

  n <- prep_expr(n)

  .by <- tidyselect_names(.df, c({{ .by }}, {{ by }}))

  i <- expr(rlang::seq2(.N - min(!!n, .N) + 1, .N))

  dt_expr <- call2_i(.df, i, .by)

  eval_tidy(dt_expr, env = dt_env)
}

#' @export
slice_tail.grouped_tt <- function(.df, n = 5, ..., .by = NULL, by = NULL) {
  .by <- group_vars(.df)
  out <- ungroup(.df)
  out <- slice_tail(out, {{ n }}, .by = any_of(.by))
  group_by(out, any_of(.by))
}

#' @export
slice_tail.data.frame <- function(.df, n = 5, ..., .by = NULL, by = NULL) {
  .df <- as_tidytable(.df)
  slice_tail(.df, {{ n }}, .by = {{ .by }}, by = {{ by }})
}

