#' Current group context
#'
#' @description
#' These functions give information about the "current" group.
#'
#' * `cur_group_id.()` gives a group identification number
#' * `cur_group_rows.()` gives the row indices for each group
#'
#' Can be used inside `summarize.()`, `mutate.()`, & `filter.()`
#'
#' @export
#'
#' @examples
#' df <- data.table(
#'   x = 1:5,
#'   y = c("a", "a", "a", "b", "b")
#' )
#'
#' df %>%
#'   mutate.(group_id = cur_group_id.(),
#'           .by = y)
#'
#' df %>%
#'   mutate.(group_rows = cur_group_rows.(),
#'           .by = y)
cur_group_id. <- function() {
  abort("cur_group_id.() should only be used inside tidytable verbs")
}

#' @export
#' @rdname cur_group_id.
cur_group_rows. <- function() {
  abort("cur_group_rows.() should only be used inside tidytable verbs")
}
