#' Relocate a column to a new position
#'
#' @description
#' Move a column or columns to a new position
#'
#' Supports enhanced selection
#'
#' @param .data A data.frame or data.table
#' @param ... A selection of columns
#' @param .before Column to move selection before
#' @param .after Column to move selection after
#'
#' @export
#'
#' @examples
#' test_df <- data.table::data.table(
#'   a = 1:5,
#'   b = 1:5,
#'   c = c("a","a","a","b","b"),
#'   d = c("a","a","a","b","b"))
#'
#' test_df %>%
#'   dt_relocate(c, .before = b)
#'
#' test_df %>%
#'   dt_relocate(is.numeric, .after = c)
dt_relocate <- function(.data, ..., .before = NULL, .after = NULL) {
  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  .before <- enexpr(.before)
  .after <- enexpr(.after)

  if  (!is.null(.before) && !is.null(.after))
    stop("Must supply only one of `.before` and `.after`")

  to_move <- dots_selector_i(.data, ...)

  if (!is.null(.before)) {
    where <- vec_selector_i(.data, !!.before)
    to_move <- c(setdiff(to_move, where), where)
  } else if (!is.null(.after)) {
    where <- vec_selector_i(.data, !!.after)
    to_move <- c(where, setdiff(to_move, where))
  } else {
    where <- 1L
    to_move <- union(to_move, where)
  }

  lhs <- setdiff(seq2(1, min(where) - 1), to_move)
  rhs <- setdiff(seq2(max(where) + 1, ncol(.data)), to_move)

  select_index <- unique(c(lhs, to_move, rhs))

  select_vars <- names(.data)[select_index]

  .data[, ..select_vars]
}
