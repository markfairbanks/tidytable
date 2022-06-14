#' Fast if_else
#'
#' @description
#' `if_else.()` utilizes `data.table::fifelse()` in the background, but automatically
#' converts `NA`s to their proper type.
#'
#' @param condition Conditions to test on
#' @param true Values to return if conditions evaluate to `TRUE`
#' @param false Values to return if conditions evaluate to `FALSE`
#' @param missing Value to return if an element of test is `NA`
#'
#' @export
#'
#' @examples
#' x <- 1:5
#' if_else.(x < 3, 1, 0)
#'
#' # Can also be used inside of mutate.()
#' df <- data.table(x = x)
#'
#' df %>%
#'   mutate.(new_col = if_else.(x < 3, 1, 0))
if_else. <- function(condition, true, false, missing = NA) {
  ptype <- vec_ptype_common(true, false, missing)

  args <- vec_cast_common(true = true, false = false, missing = missing, .to = ptype)

  fifelse(condition, args$true, args$false, args$missing)
}

#' Fast ifelse
#'
#' @description
#' `ifelse.()` utilizes `data.table::fifelse()` in the background, but automatically
#' converts `NA`s to their proper type.
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
#' ifelse.(x < 3, 1, 0)
#'
#' # Can also be used inside of mutate.()
#' df <- data.table(x = x)
#'
#' df %>%
#'   mutate.(new_col = ifelse.(x < 3, 1, 0))
ifelse. <- function(conditions, true, false, na = NA) {
  if_else.(conditions, true, false, na)
}
