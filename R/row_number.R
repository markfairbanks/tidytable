#' Return row number
#'
#' @description
#' Returns row number. This function is designed to work inside of `mutate.()`
#'
#' @md
#' @export
#'
#' @examples
#' test_df <- data.table(x = c(1,1,1))
#'
#' test_df %>%
#'   mutate.(row = row_number.())
row_number. <- function() {
  eval_expr(1:.N, env = caller_env())
}
