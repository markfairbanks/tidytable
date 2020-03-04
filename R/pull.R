#' Pull out a single variable
#'
#' @description
#' Pull a single variable from a data.table as a vector.
#'
#' @param .data A data.frame or data.table
#' @param var The column to pull from the data.table
#'
#' @export
#'
#' @examples
#' example_dt <- data.table::data.table(
#'   x = c(1,2,3),
#'   y = c(4,5,6))
#'
#' example_dt %>%
#'   dt_pull(y)
dt_pull <- function(.data, var = NULL) {
  UseMethod("dt_pull")
}

#' @export
dt_pull.tidytable <- function(.data, var = NULL) {

  var <- enexpr(var)
  if (is.null(var)) stop("var must be supplied")

  eval_tidy(expr(
    .data[, !!var]
  ))
}

#' @export
dt_pull.data.frame <- function(.data, var = NULL) {
  .data <- as_tidytable(.data)
  var <- enexpr(var)

  dt_pull(.data, !!var)
}
