#' Return row number
#'
#' @description
#' This function is designed to work inside of `mutate.()`
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
  eval_tidy(expr(1:.N), env = caller_env())
}

#' @export
#' @rdname dt_verb
#' @inheritParams row_number.
dt_row_number <- function() {
  deprecate_warn("0.5.2", "tidytable::dt_row_number()", "row_number.()")

  row_number.()
}
