#' Number of observations in each group
#'
#' @description
#' Helper function that can be used to find counts by group.
#'
#' Can be used inside `summarize()`, `mutate()`, & `filter()`
#'
#' @export
#'
#' @examples
#' df <- data.table(
#'   x = 1:3,
#'   y = 4:6,
#'   z = c("a","a","b")
#'  )
#'
#' df %>%
#'   summarize(count = n(), .by = z)
n <- function() {
  abort("n() should only be used inside tidytable verbs")
}

#' @export
#' @keywords internal
#' @inherit n title description examples
n. <- n
