#' Rowwise tidytable creation
#'
#' @description
#' Create a tidytable using a rowwise setup.
#'
#' @param ... Column names as formulas, values below. See example.
#' @export
#'
#' @examples
#' tribble(
#'   ~ x, ~ y,
#'   "a", 1,
#'   "b", 2,
#'   "c", 3
#' )
tribble <- function(...) {
  dots <- list2(...)
  is_name <- map_lgl(dots, is_bare_formula)
  col_names <- map(dots[is_name], ~ as_name(f_rhs(.x)))
  num_cols <- length(col_names)
  dots <- dots[!is_name]
  dots_length <- length(dots)
  l <- vector("list", num_cols)
  l <- set_names(l, col_names)
  for (i in seq_len(num_cols)) {
    locs <- seq(i, dots_length, num_cols)
    l[[i]] <- vec_c(!!!dots[locs])
  }
  new_tidytable(l)
}
