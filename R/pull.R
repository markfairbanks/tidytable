#' Pull out a single variable
#'
#' @description
#' Pull a single variable from a data.table as a vector.
#'
#' @param .df A data.frame or data.table
#' @param var The column to pull from the data.table as:
#' * a variable name
#' * a positive integer giving the column position
#' * a negative integer giving the column position counting from the right
#'
#' @export
#'
#' @examples
#' test_df <- data.table(
#'   x = 1:3,
#'   y = 1:3
#' )
#'
#' # Grab column by name
#' test_df %>%
#'   pull.(y)
#'
#' # Grab column by position
#' test_df %>%
#'   pull.(1)
#'
#' # Defaults to last column
#' test_df %>%
#'   pull.()
pull. <- function(.df, var = -1) {
  UseMethod("pull.")
}

#' @export
pull..data.frame <- function(.df, var = -1) {
  var_list <- as.list(seq_along(.df))

  names(var_list) <- names(.df)

  .var <- eval_tidy(enquo(var), var_list)

  if (.var < 0) .var <- length(var_list) + .var + 1

  .df[[.var]]
}
