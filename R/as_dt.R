#' Safely work with data.tables
#'
#' @description
#' Copies a data.table so you can "safely" work on it or converts a data.frame to a data.table.
#'
#' @param .data A data.frame or data.table
#'
#' @return A data.table
#' @export
#'
#' @import data.table
#' @examples
#' example_df <- data.frame(x = 1:10)
#'
#' example_df %>%
#'   as_dt() %>%
#'   dt_mutate(double_x = x * 2)
as_dt <- function(.data) {
  if (is.data.table(.data)) {
    copy(.data)
  } else {
    as.data.table(.data)
  }
}
