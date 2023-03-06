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
  .df <- .df_as_tidytable(.df)

  dots <- enquos(...)

  check_across(dots, "distinct")

  if (length(dots) == 0) {
    out <- unique(.df)
  } else {
    cols <- tidyselect_locs(.df, ...)

    out <- unique(.df, by = cols)

    if (!.keep_all) {
      cols_keep <- unname(cols)
      out <- select(out, any_of(cols_keep))
    }

    .is_named <- have_name(dots)

    if (any(.is_named)) {
      named_dots <- dots[.is_named]

      out <- rename(out, !!!named_dots)
    }
  }

  out
}

#' @export
#' @keywords internal
#' @inherit distinct
distinct. <- function(.df, ..., .keep_all = FALSE) {
  deprecate_dot_fun()
  distinct(.df, ..., .keep_all = .keep_all)
}
