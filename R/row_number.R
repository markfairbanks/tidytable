#' Return row number
#'
#' @description
#' Returns row number. This function is designed to work inside of `mutate.()`
#'
#' @md
#' @export
#'
#' @examples
#' df <- data.table(x = rep(1, 3), y = c("a", "a", "b"))
#'
#' df %>%
#'   mutate.(row = row_number.())
#'
#' # The dplyr version `row_number()` also works
#' df %>%
#'   mutate.(row = row_number())
row_number. <- function() {
  abort("row_number.() should only be used inside tidytable verbs")
}
