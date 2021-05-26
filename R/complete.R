#' Complete a data.table with missing combinations of data
#'
#' @description
#' Turns implicit missing values into explicit missing values.
#'
#' @param .df A data.frame or data.table
#' @param ... Columns to expand
#' @param fill A named list of values to fill NAs with.
#' @param .by Columns to group by
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
#'   complete.(x, y, fill = list(z = 10))
complete. <- function(.df, ..., fill = list(), .by = NULL) {
  UseMethod("complete.")
}

#' @export
complete..tidytable <- function(.df, ..., fill = list(), .by = NULL) {
  dots <- enquos(...)
  dots <- dots[!map_lgl.(dots, quo_is_null)]
  if (length(dots) == 0) return(.df)

  full_df <- expand.(.df, !!!dots, .by = {{ .by }})

  if (is_empty(full_df)) return(.df)

  full_df <- full_join.(full_df, .df, by = names(full_df))
  full_df <- replace_na.(full_df, replace = fill)

  full_df
}

#' @export
complete..data.frame <- function(.df, ..., fill = list(), .by = NULL) {
  .df <- as_tidytable(.df)
  complete.(.df, ..., fill = fill, .by = {{ .by }})
}
