#' Combine values from multiple columns
#' 
#' @description
#' `c_across.()` works inside of `mutate_rowwise.()`. It uses tidyselect so
#' you can easily select multiple variables.
#'
#' @param cols Columns to transform.
#'
#' @export
#'
#' @examples
#' test_df <- data.table(x = runif(6), y = runif(6), z = runif(6))
#'
#' test_df %>%
#'   mutate_rowwise.(row_mean = mean(c_across.(x:z)))
c_across. <- function(cols = everything()) {
  abort("c_across.() can only be used inside of mutate_rowwise.()")
}