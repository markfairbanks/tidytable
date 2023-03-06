#' @export
#' @rdname slice
slice_head <- function(.df, n = 5, ..., .by = NULL, by = NULL) {
  .df <- .df_as_tidytable(.df)

  if (is_ungrouped(.df)) {
    tt_slice_head(.df, {{ n }}, .by = {{ .by }}, by = {{ by }})
  } else {
    .by <- group_vars(.df)
    tt_slice_head(.df, {{ n }}, .by = all_of(.by))
  }
}

#' @export
#' @keywords internal
#' @inherit slice
slice_head. <- function(.df, n = 5, ..., .by = NULL, by = NULL) {
  deprecate_dot_fun()
  slice_head(.df, {{ n }}, .by = {{ .by }}, by = {{ by }})
}

tt_slice_head <- function(.df, n = 5, ..., .by = NULL, by = NULL) {
  n <- enquo(n)

  dt_env <- get_dt_env(n)

  n <- prep_expr(n)

  .by <- tidyselect_names(.df, c({{ .by }}, {{ by }}))

  i <- expr(seq_len(min(!!n, .N)))

  dt_expr <- call2_i(.df, i, .by)

  eval_tidy(dt_expr, env = dt_env)
}

#' @export
#' @rdname slice
slice_tail <- function(.df, n = 5, ..., .by = NULL, by = NULL) {
  .df <- .df_as_tidytable(.df)

  if (is_ungrouped(.df)) {
    tt_slice_tail(.df, {{ n }}, .by = {{ .by }}, by = {{ by }})
  } else {
    .by <- group_vars(.df)
    tt_slice_tail(.df, {{ n }}, .by = all_of(.by))
  }
}

#' @export
#' @keywords internal
#' @inherit slice
slice_tail. <- function(.df, n = 5, ..., .by = NULL, by = NULL) {
  deprecate_dot_fun()
  slice_tail(.df, {{ n }}, .by = {{ .by }}, by = {{ by }})
}

tt_slice_tail <- function(.df, n = 5, ..., .by = NULL, by = NULL) {
  n <- enquo(n)

  dt_env <- get_dt_env(n)

  n <- prep_expr(n)

  .by <- tidyselect_names(.df, c({{ .by }}, {{ by }}))

  i <- expr(rlang::seq2(.N - min(!!n, .N) + 1, .N))

  dt_expr <- call2_i(.df, i, .by)

  eval_tidy(dt_expr, env = dt_env)
}

