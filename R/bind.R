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
#' @import data.table
#' @export
#'
#' @examples
#'
#' example_dt %>%
#'   dt_bind_rows(another_dt)
#'
#' example_dt %>%
#'   dt_bind_cols(another_dt)
dt_bind_rows <- data.table:::rbind.data.table

#' @export
#' @inherit dt_bind_rows
dt_bind_cols <- data.table:::cbind.data.table
