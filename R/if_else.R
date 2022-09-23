#' Fast if_else
#'
#' @description
#' Fast version of `base::ifelse()`.
#'
#' @param condition Conditions to test on
#' @param true Values to return if conditions evaluate to `TRUE`
#' @param false Values to return if conditions evaluate to `FALSE`
#' @param missing Value to return if an element of test is `NA`
#' @inheritParams rlang::args_dots_empty
#' @param ptype Optional ptype to override output type
#' @param size Optional size to override output size
#'
#' @export
#'
#' @examples
#' x <- 1:5
#' if_else(x < 3, 1, 0)
#'
#' # Can also be used inside of mutate()
#' df <- data.table(x = x)
#'
#' df %>%
#'   mutate(new_col = if_else(x < 3, 1, 0))
if_else <- function(condition, true, false, missing = NA, ..., ptype = NULL, size = NULL) {
  check_dots_empty0(...)

  args <- vec_cast_common(true = true, false = false, missing = missing, .to = ptype)

  out <- fifelse(condition, args$true, args$false, args$missing)

  if (!is.null(size)) {
    out <- vec_recycle(out, size)
  }

  out
}

#' @export
#' @keywords internal
#' @inherit if_else
if_else. <- if_else

#' Fast ifelse
#'
#' @description
#' Fast version of `base::ifelse()`.
#'
#' @param conditions Conditions to test on
#' @param true Values to return if conditions evaluate to `TRUE`
#' @param false Values to return if conditions evaluate to `FALSE`
#' @param na Value to return if an element of test is `NA`
#'
#' @export
#'
#' @examples
#' x <- 1:5
#' ifelse(x < 3, 1, 0)
#'
#' # Can also be used inside of mutate()
#' df <- data.table(x = x)
#'
#' df %>%
#'   mutate(new_col = ifelse(x < 3, 1, 0))
#' @keywords internal
ifelse. <- function(conditions, true, false, na = NA) {
  if_else(conditions, true, false, na)
}
