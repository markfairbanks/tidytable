#' Convert a vector to a data.table/tidytable
#'
#' @description
#' Converts named and unnamed vectors to a data.table/tidytable.
#'
#' @param x A vector
#' @param name Name of the column that stores the names. If `name = NULL`,
#' a one-column tidytable will be returned.
#' @param value Name of the column that stores the values.
#'
#' @export
#'
#' @examples
#' vec <- 1:3
#' names(vec) <- letters[1:3]
#'
#' enframe.(vec)
enframe. <- function(x, name = "name", value = "value") {
  if (is.null(x)) x <- logical()

  if (is.null(value)) abort("`value` can't be NULL")

  if (is.null(name)) {
    l <- list(unname(x))
  } else if (is.null(names(x))) {
    l <- list(seq_along(x), x)
  } else {
    l <- list(names(x), unname(x))
  }

  names(l) <- c(name, value)

  new_tidytable(l)
}
