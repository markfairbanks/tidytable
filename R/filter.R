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
filter..tidytable <- function(.df, ..., .by = NULL) {
  .by <- enquo(.by)

  dots <- enquos(...)
  if (length(dots) == 0) return(.df)

  check_filter(dots)

  dt_env <- get_dt_env(dots)

  dots <- prep_exprs(dots, .df, !!.by)

  i <- call_reduce(dots, "&")

  if (quo_is_null(.by)) {
    dt_expr <- call2_i(.df, i)

    .df <- eval_tidy(dt_expr, env = dt_env)
  } else {
    .by <- tidyselect_names(.df, !!.by)

    j <- expr(.I[!!i])

    dt_expr <- call2_fast_by_i(.df, j, .by)

    .df <- eval_tidy(dt_expr, env = dt_env)
  }

  .df
}

#' @export
filter..data.frame <- function(.df, ..., .by = NULL) {
  .df <- as_tidytable(.df)
  filter.(.df, ..., .by = {{ .by }})
}

check_filter <- function(dots) {
  named_bool <- have_name(dots)

  if (any(named_bool)) {
    named_dots <- dots[named_bool]

    i <- which(named_bool)[[1]]
    dot <- as_label(quo_get_expr(named_dots[[1]]))
    dot_name <- names(named_dots[1])

    abort(c(
      glue("Problem with `filter.()` input `..{i}`."),
      x = glue("Input `..{i}` is named."),
      i = glue("This usually means that you've used `=` instead of `==`."),
      i = glue("Did you mean `{dot_name} == {dot}`?")
    ))
  }
}
