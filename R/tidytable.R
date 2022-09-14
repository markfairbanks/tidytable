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
    dots <- list()
  } else {
    dots <- dots_list(...)
    dots <- map(dots, eval_tidy)
    if (any(have_name(dots) & map_lgl(dots, is.data.frame))) {
      abort("data frame inputs must be unnamed")
    }
    dots <- df_list(!!!dots, .name_repair = .name_repair)
  }

  new_tidytable(dots)
}
