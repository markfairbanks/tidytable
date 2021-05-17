#' Filter rows on one or more conditions
#'
#' @description
#' Filters a dataset to choose rows where conditions are true.
#'
#' @param .df A data.frame or data.table
#' @param ... Conditions to filter by
#' @param .by Columns to group by if filtering with a summary function
#'
#' @export
#'
#' @examples
#' test_df <- tidytable(
#'   a = 1:3,
#'   b = 4:6,
#'   c = c("a", "a", "b")
#' )
#'
#' test_df %>%
#'   filter.(a >= 2, b >= 4)
#'
#' test_df %>%
#'   filter.(b <= mean(b), .by = c)
filter. <- function(.df, ..., .by = NULL) {
  UseMethod("filter.")
}

#' @export
#' @export
filter..tidytable <- function(.df, ..., .by = NULL) {
  .df <- as_tidytable(.df)

  .by <- enquo(.by)

  dots <- enquos(...)
  if (length(dots) == 0) return(.df)

  dt_env <- build_dt_env(dots)

  dots <- prep_exprs(dots, .df, !!.by)

  i <- expr(Reduce('&', list(!!!dots)))

  if (quo_is_null(.by)) {
    dt_expr <- call2_i(.df, i)

    .df <- eval_tidy(dt_expr, env = dt_env)
  } else {
    .by <- tidyselect_names(.df, !!.by)

    j <- expr(.I[!!i])

    dt_expr <- call2_j(.df, j, .by)
    dt_expr <- call2("$", dt_expr, expr(V1))
    dt_expr <- call2_i(.df, dt_expr)

    .df <- eval_tidy(dt_expr, env = dt_env)
  }

  .df
}

#' @export
filter..data.frame <- function(.df, ..., .by = NULL) {
  .df <- as_tidytable(.df)
  filter.(.df, ..., .by = {{ .by }})
}
