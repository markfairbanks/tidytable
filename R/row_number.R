#' Return row number
#'
#' @description
#' This function is designed to work inside of `mutate.()`
#'
#' @md
#' @export
#'
#' @examples
#' test_df <- data.table::data.table(x = c(1,1,1))
#'
#' test_df %>%
#'   mutate.(row = row_number.())
row_number. <- function() {
  eval_tidy(expr(1:.N), env = caller_env())
}

#' @export
#' @rdname row_number.
dt_row_number <- row_number.
