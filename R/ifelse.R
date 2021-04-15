#' Fast ifelse
#'
#' @description
#' `ifelse.()` utilizes `data.table::fifelse()` in the background, but automatically
#' converts NAs to their proper type.
#'
#' @param conditions Conditions to test on
#' @param true Values to return if conditions evaluate to TRUE
#' @param false Values to return if conditions evaluate to FALSE
#' @param na Value to return if an element of test is NA.
#'
#' @md
#' @export
#'
#' @examples
#' x <- 1:5
#' ifelse.(x < 3, 1, 0)
#'
#' # Can also be used inside of mutate.()
#' test_df <- data.table(x = x)
#'
#' test_df %>%
#'   mutate.(new_col = ifelse.(x < 3, 1, 0))
ifelse. <- function(conditions, true, false, na = NA) {
  ptype <- vec_ptype_common(true, false, na)

  args <- vec_cast_common(true = true, false = false, na = na, .to = ptype)

  fifelse(conditions, args$true, args$false, args$na)
}
