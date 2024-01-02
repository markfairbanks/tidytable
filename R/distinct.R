#' Select distinct/unique rows
#'
#' @description
#' Retain only unique/distinct rows from an input df.
#'
#' @param .df A data.frame or data.table
#' @param ... Columns to select before determining uniqueness. If omitted, will use all columns.
#' `tidyselect` compatible.
#' @param .keep_all Only relevant if columns are provided to ... arg.
#' This keeps all columns, but only keeps the first row of each distinct
#' values of columns provided to ... arg.
#'
#' @export
#'
#' @examples
#' df <- tidytable(
#'   x = 1:3,
#'   y = 4:6,
#'   z = c("a", "a", "b")
#' )
#'
#' df %>%
#'   distinct()
#'
#' df %>%
#'   distinct(z)
distinct <- function(.df, ..., .keep_all = FALSE) {
  UseMethod("distinct")
}

#' @export
distinct.tidytable <- function(.df, ..., .keep_all = FALSE) {
  if (missing(...)) {
    out <- vec_unique(.df)
  } else {
    dots <- enquos(...)

    check_no_across(dots)

    cols <- tidyselect_locs(.df, !!!dots)

    if (.keep_all) {
      locs <- vec_unique_loc(select(.df, any_of(cols)))
      out <- vec_slice(.df, locs)
      out <- rename(out, any_of(cols))
    } else {
      out <- select(.df, any_of(cols))
      out <- vec_unique(out)
    }
  }

  out
}

#' @export
distinct.data.frame <- function(.df, ..., .keep_all = FALSE) {
  .df <- as_tidytable(.df)
  distinct(.df, ..., .keep_all = .keep_all)
}
