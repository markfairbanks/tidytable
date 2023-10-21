#' Fill in missing values with previous or next value
#'
#' @description
#' Fills missing values in the selected columns using the next or previous entry. Can be done by group.
#'
#' Supports tidyselect
#'
#' @param .df A data.frame or data.table
#' @param ... A selection of columns. `tidyselect` compatible.
#' @param .direction Direction in which to fill missing values.
#' Currently "down" (the default), "up", "downup" (first down then up), or "updown" (first up and then down)
#' @param .by Columns to group by when filling should be done by group
#'
#' @export
#'
#' @examples
#' df <- data.table(
#'   a = c(1, NA, 3, 4, 5),
#'   b = c(NA, 2, NA, NA, 5),
#'   groups = c("a", "a", "a", "b", "b")
#' )
#'
#' df %>%
#'   fill(a, b)
#'
#' df %>%
#'   fill(a, b, .by = groups)
#'
#' df %>%
#'   fill(a, b, .direction = "downup", .by = groups)
fill <- function(.df, ...,
                 .direction = c("down", "up", "downup", "updown"),
                 .by = NULL) {
  UseMethod("fill")
}

#' @export
fill.data.frame <- function(.df, ...,
                            .direction = c("down", "up", "downup", "updown"),
                            .by = NULL) {
  .direction <- arg_match(.direction)

  mutate(.df, across(c(...), ~ vec_fill_missing(.x, .direction)), .by = {{ .by }})
}

