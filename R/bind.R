#' Bind data.tables by row and column
#'
#' @description
#'
#' Simple alias to data.table implementations of `rbind()` and `cbind()`
#'
#' @usage
#' dt_bind_rows(...)
#' dt_bind_cols(...)
#'
#' @param ... Data frames to combine.
#'
#'
#' @import data.table
#' @export
#' @md
#'
#' @examples
#'
#' example_dt %>%
#'   dt_bind_rows(another_dt)
#'
#' example_dt %>%
#'   dt_bind_cols(another_dt)
dt_bind_rows <- function(...) {
  rbind(...)
}

#' @export
#' @inherit dt_bind_rows
dt_bind_cols <- function(...) {
  cbind(...)
}
