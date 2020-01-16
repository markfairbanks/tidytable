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
#' @param .data A data frame to bind
#' @param ... Data frames to bind
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
dt_bind_rows <- function(.data, ...) {
  dots <- enexprs(...)
  dots <- dt_map(dots, eval)
  dots <- append(list(.data), dots)

  if (!all(dt_map_lgl(dots, is.data.frame)))
    stop("All inputs must be a data.frame or data.table")

  if (!all(dt_map_lgl(dots, is.data.table)))
    dots <- dt_map(dots, as.data.table)

  rbindlist(dots)
}

#' @export
#' @inherit dt_bind_rows
dt_bind_cols <- function(.data, ...) {
  dots <- enexprs(...)
  dots <- dt_map(dots, eval)
  dots <- append(list(.data), dots)

  if (!all(dt_map_lgl(dots, is.data.frame)))
    stop("All inputs must be a data.frame or data.table")

  if (!all(dt_map_lgl(dots, is.data.table)))
    dots <- dt_map(dots, as.data.table)

  do.call(cbind, dots)
}
