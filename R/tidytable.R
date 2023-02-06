#' Build a data.table/tidytable
#'
#' @description
#' Constructs a data.table, but one with nice printing features.
#'
#' @param ... A set of name-value pairs
#' @param .name_repair Treatment of duplicate names. See `?vctrs::vec_as_names` for options/details.
#'
#' @export
#'
#' @examples
#' tidytable(x = 1:3, y = c("a", "a", "b"))
tidytable <- function(..., .name_repair = "unique") {
  if (missing(...)) {
    new_tidytable()
  } else {
    dots <- dots_list(..., .named = TRUE)
    dots <- map(dots, eval_tidy)
    unpack(dots, .name_repair)
  }
}
