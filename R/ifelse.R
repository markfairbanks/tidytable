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

  if (length(false) == 1)
    if (is.na(false)) false <- as(false, class(true))

  if (length(true) == 1)
    if(is.na(true)) true <- as(true, class(false))

  fifelse(conditions, true, false, na = na)
}

#' @export
#' @rdname ifelse.
dt_ifelse <- ifelse.
