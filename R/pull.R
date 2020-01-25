#' Pull out a single variable
#'
#' @description
#' Pull a single variable from a data.table as a vector.
#'
#' @param .data A data.frame or data.table
#' @param var The column to pull from the data.table
#'
#' @return A vector
#' @import data.table
#' @export
#'
#' @examples
#' library(data.table)
#'
#' example_dt <- data.table(x = c(1,2,3), y = c(4,5,6))
#'
#' example_dt %>%
#'   dt_pull(y)
#'
#' @import data.table
#' @importFrom rlang enexprs
#' @importFrom rlang eval_tidy
#' @importFrom rlang expr
#' @importFrom rlang enexpr
dt_pull <- function(.data, var = NULL) {

  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  var <- enexpr(var)
  if (is.null(var)) stop("var must be supplied")

  eval_tidy(expr(
    .data[, !!var]
  ))
}
