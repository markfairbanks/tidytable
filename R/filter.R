#' Filter rows on one or more conditions
#'
#' @description
#' Filters a dataset to choose rows where conditions are true.
#'
#' @param .df A data.frame or data.table
#' @param ... Conditions to filter by
#' @param .by Columns to group by if filtering with a summary function
#' @param by This argument has been renamed to .by and is deprecated
#'
#' @export
#'
#' @examples
#' test_df <- tidytable(
#'   a = c(1,2,3),
#'   b = c(4,5,6),
#'   c = c("a","a","b"))
#'
#' test_df %>%
#'   filter.(a >= 2, b >= 4)
#'
#' test_df %>%
#'   filter.(b <= mean(b), .by = c)
filter. <- function(.df, ..., .by = NULL, by = NULL) {
  UseMethod("filter.")
}

#' @export
filter..data.frame <- function(.df, ..., .by = NULL, by = NULL) {

  .df <- as_tidytable(.df)

  .by <- check_dot_by(enquo(.by), enquo(by), "filter.")

  dots <- enquos(...)

  if (quo_is_null(.by)) {

    .df <- eval_quo(
      .df[Reduce('&', list(!!!dots))]
    )

  } else {
    .by <- select_vec_chr(.df, !!.by)

    col_order <- names(.df)

    .df <- eval_quo(
      .df[, .SD[Reduce('&', list(!!!dots))], by = .by]
    )

    setcolorder(.df, col_order)
  }

  .df
}

#' @export
#' @rdname filter.
dt_filter <- function(.df, ..., .by = NULL, by = NULL) {
  deprecate_soft("0.5.2", "tidytable::dt_filter()", "filter.()")

  .by <- check_dot_by(enquo(.by), enquo(by))

  filter.(.df, ..., .by = {{ .by }})
}
