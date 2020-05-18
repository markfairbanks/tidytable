#' Select variables with a function
#'
#' This [selection helper][language] selects the variables for which a
#' function returns `TRUE`.
#'
#' @param fn A function that returns `TRUE` or `FALSE` (technically, a
#'   _predicate_ function). Can also be a purrr-like formula.
#'
#' @section Examples:
#'
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#'
#' Selection helpers can be used in functions like `dplyr::select()`
#' or `tidyr::pivot_longer()`. Let's first attach the tidyverse:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' library(tidyverse)
#'
#' # For better printing
#' iris <- as_tibble(iris)
#' ```
#'
#' `where()` takes a function and returns all variables for which the
#' function returns `TRUE`:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' is.factor(iris[[4]])
#'
#' is.factor(iris[[5]])
#'
#' iris %>% select(where(is.factor))
#'
#' is.numeric(iris[[4]])
#'
#' is.numeric(iris[[5]])
#'
#' iris %>% select(where(is.numeric))
#' ```
#'
#'
#' ## The formula shorthand
#'
#' You can use purrr-like formulas as a shortcut for creating a
#' function on the spot. These expressions are equivalent:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' iris %>% select(where(is.numeric))
#'
#' iris %>% select(where(function(x) is.numeric(x)))
#'
#' iris %>% select(where(~ is.numeric(.x)))
#' ```
#'
#' The shorthand is useful for adding logic inline. Here we select all
#' numeric variables whose mean is greater than 3.5:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' iris %>% select(where(~ is.numeric(.x) && mean(.x) > 3.5))
#' ```
#'
#' @name where
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
