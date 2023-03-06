#' Create a data.table from all combinations of inputs
#'
#' @description
#' Create a data.table from all combinations of inputs
#'
#' @param ... Variables to get combinations of
#' @param .name_repair Treatment of problematic names. See `?vctrs::vec_as_names` for options/details
#'
#' @export
#'
#' @examples
#' x <- 1:2
#' y <- 1:2
#'
#' expand_grid(x, y)
#'
#' expand_grid(stuff = x, y)
expand_grid <- function(..., .name_repair = "check_unique") {
  dots <- dots_list(..., .named = TRUE)

  out <- vec_expand_grid(!!!dots, .name_repair = "minimal")

  unpack(out, .name_repair)
}

#' @export
#' @keywords internal
#' @inherit expand_grid
expand_grid. <- function(..., .name_repair = "check_unique") {
  deprecate_dot_fun()
  expand_grid(..., .name_repair = .name_repair)
}
