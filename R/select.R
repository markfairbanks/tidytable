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

  out <- new_data_frame(.df)[locs]

  out <- df_set_names(out, names(locs))

  tidytable_restore(out, .df)
}

#' @export
select..data.frame <- function(.df, ...) {
  .df <- as_tidytable(.df)
  select.(.df, ...)
}
