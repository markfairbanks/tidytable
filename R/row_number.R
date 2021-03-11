#' Return row number
#'
#' @description
#' Returns row number. This function is designed to work inside of `mutate.()`
#'
#' @md
#' @export
#'
#' @examples
#' test_df <- data.table(x = rep(1, 3))
#'
#' test_df %>%
#'   mutate.(row = row_number.())
row_number. <- function() {
  abort("row_number.() should only be used inside tidytable verbs")
}
