#' Complete a data.table with missing combinations of data
#'
#' @description
#' Turns implicit missing values into explicit missing values.
#'
#' @param .df A data.frame or data.table
#' @param ... Columns to expand
#' @param .fill A named list of values to fill NAs with.
#'
#' @export
#'
#' @examples
#' test_df <- data.table(x = 1:2, y = 1:2, z = 3:4)
#'
#' test_df %>%
#'   complete.(x, y)
#'
#' test_df %>%
#'   complete.(x, y, .fill = list(z = 10))
complete. <- function(.df, ..., .fill = list()) {
  UseMethod("complete.")
}

#' @export
complete..data.frame <- function(.df, ..., .fill = list()) {

  # No need for as_tidytable() conversion
  # Step is covered by other functions

  full <- expand.(.df, ...)

  if (is_empty(full)) {
    return(.df)
  }

  full <- full_join.(full, .df, by = names(full))
  full <- replace_na.(full, replace = .fill)

  as_tidytable(full)
}
