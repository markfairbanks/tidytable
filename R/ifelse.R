#' Vectorized if
#'
#' @description
#' `ifelse.()` utilizes `data.table::fifelse()` in the background, but automatically
#' converts NAs to their proper type
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
#' x <- c(1:4, 3:2, 1:4)
#' ifelse.(x > 2L, x, x - 1L)
#'
#' # Can also be used inside of mutate.()
#' test_df <- data.table::data.table(x = x)
#'
#' test_df %>%
#'   mutate.(new_col = ifelse.(x > 2L, NA, x - 1L))
ifelse. <- function(conditions, true, false, na = NA) {

  true <- true
  false <- false
  na <- na

  vec_assert(conditions, logical())
  ptype <- vec_ptype_common(true, false, na)

  true <- vec_cast(true, ptype)
  false <- vec_cast(false, ptype)
  na <- vec_cast(na, ptype)

  fifelse(conditions, true, false, na = na)
}

#' @export
#' @rdname ifelse.
dt_ifelse <- ifelse.
