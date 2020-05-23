#' Filter rows on one or more conditions
#'
#' @description
#' Filters a dataset to choose rows where conditions are true.
#'
#' @param .df A data.frame or data.table
#' @param ... Conditions to filter by
#' @param by Columns to group by if filtering with a summary function
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
#'   filter.(b <= mean(b), by = c)
filter. <- function(.df, ..., by = NULL) {
  UseMethod("filter.")
}

#' @export
filter..data.frame <- function(.df, ..., by = NULL) {

  .df <- as_tidytable(.df)

  dots <- enquos(...)

  by <- enquo(by)

  if (quo_is_null(by)) {

    .df <- eval_quo(
      .df[Reduce('&', list(!!!dots))],
      .df)

  } else {
    by <- select_vec_by(.df, !!by)

    col_order <- names(.df)

    .df <- eval_quo(
      .df[, eval_quo(.SD[Reduce('&', list(!!!dots))], .SD), by = !!by],
      .df)

    setcolorder(.df, col_order)
  }
  .df
}

#' @export
#' @rdname filter.
dt_filter <- filter.
