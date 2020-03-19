#' Vectorized if
#'
#' @description
#' `dt_ifelse()` utilizes `data.table::fifelse()` in the background, but automatically
#' converts NAs to their proper type
#'
#' @param conditions Conditions to test on
#' @param true Values to return if conditions evaluate to TRUE
#' @param false Values to return if conditions evaluate to FALSE
#'
#' @md
#' @export
#'
#' @examples
#' x <- c(1:4, 3:2, 1:4)
#' dt_ifelse(x > 2L, x, x - 1L)
#'
#' # Can also be used inside of dt_mutate()
#' test_df <- data.table::data.table(x = x)
#' test_df %>%
#'   dt_mutate(new_col = dt_ifelse(x > 2L, NA, x - 1L))
dt_ifelse <- function(conditions, true, false) {

  if (is.na(true)) true <- as(true, class(false))

  dt_case(conditions, true, default = false)
}
