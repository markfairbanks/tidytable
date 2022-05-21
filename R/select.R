#' Select or drop columns
#'
#' @description
#' Select or drop columns from a data.table
#'
#' @param .df A data.frame or data.table
#' @param ... Columns to select or drop.
#' Use named arguments, e.g. new_name = old_name, to rename selected variables.
#' `tidyselect` compatible.
#'
#' @export
#'
#' @examples
#' df <- data.table(
#'   x1 = 1:3,
#'   x2 = 1:3,
#'   y = c("a", "b", "c"),
#'   z = c("a", "b", "c")
#' )
#'
#' df %>%
#'   select.(x1, y)
#'
#' df %>%
#'   select.(x1:y)
#'
#' df %>%
#'   select.(-y, -z)
#'
#' df %>%
#'   select.(starts_with("x"), z)
#'
#' df %>%
#'   select.(where(is.character), x1)
#'
#' df %>%
#'   select.(new = x1, y)
select. <- function(.df, ...) {
  UseMethod("select.")
}

#' @export
select..tidytable <- function(.df, ...) {
  locs <- tidyselect_locs(.df, ...)

  loc_dupes <- vec_duplicate_detect(locs)

  if (any(loc_dupes)) {
    # Issue #468
    out <- dt_j(.df, ..locs)
  } else {
    drop_locs <- setdiff(seq_along(.df), locs)
    if (length(drop_locs) == 0) {
      out <- fast_copy(.df)
    } else {
      out <- dt_j(.df, (drop_locs) := NULL)
    }

    col_order <- names(.df)[locs]

    out <- df_col_order(out, col_order)
  }

  df_set_names(out, names(locs))
}

#' @export
select..data.frame <- function(.df, ...) {
  .df <- as_tidytable(.df)
  select.(.df, ...)
}
