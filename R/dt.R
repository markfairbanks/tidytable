#' Pipeable data.table call
#'
#' @description
#' Pipeable data.table call
#'
#' @param ... See ?data.table:::`[.data.table`
#' @param x See ?data.table:::`[.data.table`
#' @param i See ?data.table:::`[.data.table`
#' @param j See ?data.table:::`[.data.table`
#' @param by See ?data.table:::`[.data.table`
#' @param keyby See ?data.table:::`[.data.table`
#' @param with See ?data.table:::`[.data.table`
#' @param nomatch See ?data.table:::`[.data.table`
#' @param mult See ?data.table:::`[.data.table`
#' @param roll See ?data.table:::`[.data.table`
#' @param rollends See ?data.table:::`[.data.table`
#' @param which See ?data.table:::`[.data.table`
#' @param .SDcols See ?data.table:::`[.data.table`
#' @param verbose See ?data.table:::`[.data.table`
#' @param allow.cartesian See ?data.table:::`[.data.table`
#' @param drop See ?data.table:::`[.data.table`
#' @param on See ?data.table:::`[.data.table`
#'
#' @export
#' @examples
#' example_dt <- data.table::data.table(
#'   x = c(1,2,3),
#'   y = c(4,5,6),
#'   z = c("a", "a", "b"))
#'
#' example_dt %>%
#'   as_dt() %>%
#'   dt(, ':='(double_x = x * 2)) %>%
#'   dt(order(-double_x))
dt <- data.table:::`[.data.table`
