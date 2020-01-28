#' Select variables
#'
#' @description
#' Scoped variants of `select()`
#'
#' @param .data A data.table
#' @param .predicate Predicate to specify columns for `dt_select_if()`
#'
#' @return A data.table
#' @export
#'
#' @examples
#' example_dt <- data.table::data.table(x = 1, y = 2, z = "a")
#'
#' example_dt %>% dt_select_if(is.double)
dt_select_if <- function(.data, .predicate) {

  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  .cols <- colnames(.data)[dt_map_lgl(.data, .predicate)]

  .data[, .SD, .SDcols = .cols]
}
