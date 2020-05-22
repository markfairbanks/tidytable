#' Select variables with a function
#'
#' @description
#' This selection helper selects the variables for which a
#' function returns `TRUE`.
#'
#' @param fn A function that returns `TRUE` or `FALSE` (technically, a
#'   _predicate_ function). Can also be a purrr-like formula.
#'
#' @examples
#' iris %>% select.(where(is.factor))
#'
#' iris %>% select.(where(is.numeric))
#'
#' ## The formula shorthand
#' # These expressions are equivalent:
#'
#' iris %>% select.(where(is.numeric))
#'
#' iris %>% select.(where(function(x) is.numeric(x)))
#'
#' iris %>% select.(where(~ is.numeric(.x)))
#'
#' # This shortahand is useful for adding logic inline.
#' # Here we select all numeric variables whose mean is greater than 3.5:
#' iris %>%
#'   select.(where(~ is.numeric(.x) && mean(.x) > 3.5))
#'
#' @export
where <- function(fn) {
  predicate <- as_function(fn)

  function(x, ...) {
    out <- predicate(x, ...)

    if (!rlang::is_bool(out)) {
      abort("`where()` must be used with functions that return `TRUE` or `FALSE`.")
    }

    out
  }
}
