#' as_dt() helper
#'
#' @description
#' Prevents modify-by-reference/converts a data.frame to a data.table.
#'
#' @param .data A data.frame or data.table
#' @export
#'
#' @examples
#' example_df <- data.frame(x = 1:10)
#'
#' example_df %>%
#'   as_dt() %>%
#'   dt_mutate(double_x = x * 2)
as_dt <- function(.data) {
  if (is.data.table(.data)) {
    shallow(.data)
  } else {
    as.data.table(.data)
  }
}
