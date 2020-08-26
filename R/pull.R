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
#'   x = c(1,2,3),
#'   y = c(4,5,6))
#'
#' test_df %>%
#'   pull.(y)
#'
#' test_df %>%
#'   pull.(1)
#'
#' test_df %>%
#'   pull.(-1)
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

#' @export
#' @rdname dt_verb
#' @inheritParams pull.
dt_pull <- function(.df, var = -1) {
  deprecate_warn("0.5.2", "tidytable::dt_pull()", "pull.()")

  pull.(.df, var = {{ var }})
}
