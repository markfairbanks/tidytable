#' Context functions
#'
#' @description
#' These functions give information about the "current" group.
#'
#' * `cur_data.()` gives the current data for the current group
#' * `cur_column.()` gives the name of the current column (for use in `across.()` only)
#' * `cur_group_id.()` gives a group identification number
#' * `cur_group_rows.()` gives the row indices for each group
#'
#' Can be used inside `summarize.()`, `mutate.()`, & `filter.()`
#'
#'
#' @examples
#' df <- data.table(
#'   x = 1:5,
#'   y = c("a", "a", "a", "b", "b")
#' )
#'
#' df %>%
#'   mutate.(
#'     across.(c(x, y), ~ paste(cur_column.(), .x))
#'   )
#'
#' df %>%
#'   summarize.(data = list(cur_data.()),
#'              .by = y)
#'
#' df %>%
#'   mutate.(group_id = cur_group_id.(),
#'           .by = y)
#'
#' df %>%
#'   mutate.(group_rows = cur_group_rows.(),
#'           .by = y)
#' @name context
NULL

#' @export
#' @rdname context
cur_column. <- function() {
  abort("cur_column.() should only be used inside across.()")
}

#' @export
#' @rdname context
cur_data. <- function() {
  abort("cur_data.() should only be used inside tidytable verbs")
}

#' @export
#' @rdname context
cur_group_id. <- function() {
  abort("cur_group_id.() should only be used inside tidytable verbs")
}

#' @export
#' @rdname context
cur_group_rows. <- function() {
  abort("cur_group_rows.() should only be used inside tidytable verbs")
}
