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
#' test_df <- data.table(
#'   x1 = 1:3,
#'   x2 = 1:3,
#'   y = c("a", "b", "c"),
#'   z = c("a", "b", "c")
#' )
#'
#' test_df %>%
#'   select.(x1, y)
#'
#' test_df %>%
#'   select.(x1:y)
#'
#' test_df %>%
#'   select.(-y, -z)
#'
#' test_df %>%
#'   select.(starts_with("x"), z)
#'
#' test_df %>%
#'   select.(where(is.character), x1)
#'
#' test_df %>%
#'   select.(new = x1, y)
select. <- function(.df, ...) {
  UseMethod("select.")
}

#' @export
select..tidytable <- function(.df, ...) {
  cols <- tidyselect_locs(.df, ...)

  .df <- .df[, ..cols]

  .df <- set_names(.df, names(cols))

  .df
}

#' @export
select..data.frame <- function(.df, ...) {
  .df <- as_tidytable(.df)
  select.(.df, ...)
}

globalVariables("..cols")
