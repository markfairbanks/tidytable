#' Create a data.table from all unique combinations of inputs
#'
#' @description
#' `crossing()` is similar to `expand_grid()` but de-duplicates and sorts its inputs.
#'
#' @param ... Variables to get unique combinations of
#' @param .name_repair Treatment of problematic names. See `?vctrs::vec_as_names` for options/details
#'
#' @export
#'
#' @examples
#' x <- 1:2
#' y <- 1:2
#'
#' crossing(x, y)
#'
#' crossing(stuff = x, y)
crossing <- function(..., .name_repair = "check_unique") {
  dots <- dots_list(..., .named = TRUE)
  dots <- map(dots, sort_unique)

  expand_grid(!!!dots, .name_repair = .name_repair)
}

sort_unique <- function(x) {
  if (is.factor(x)) {
    factor(levels(x), levels(x), exclude = NULL, ordered = is.ordered(x))
  } else if (is_bare_list(x)) {
    vec_unique(x)
  } else if (is.data.frame(x)) {
    setorderv(unique(as_tidytable(x)))[]
  } else {
    f_sort(vec_unique(x))
  }
}
