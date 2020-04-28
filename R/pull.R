#' Pull out a single variable
#'
#' @description
#' Pull a single variable from a data.table as a vector.
#'
#' @param .data A data.frame or data.table
#' @param var The column to pull from the data.table. If NULL, pulls the last column.
#'
#' @export
#'
#' @examples
#' example_dt <- data.table::data.table(
#'   x = c(1,2,3),
#'   y = c(4,5,6))
#'
#' example_dt %>%
#'   pull.(y)
pull. <- function(.data, var = NULL) {
  UseMethod("pull.")
}

#' @export
pull..data.frame <- function(.data, var = NULL) {

  .data <- as_tidytable(.data)

  var <- enexpr(var)
  if (is.null(var)) var <- sym(names(.data)[ncol(.data)])

  # Base R translation is faster than data.table
  eval_expr(
    '$'(.data, !!var)
  )
}

#' @export
#' @rdname pull.
dt_pull <- pull.
