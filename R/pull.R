#' Pull out a single variable
#'
#' @description
#' Pull a single variable from a data.table as a vector.
#'
#' @param .df A data.frame or data.table
#' @param var The column to pull from the data.table. If NULL, pulls the last column.
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
pull. <- function(.df, var = NULL) {
  UseMethod("pull.")
}

#' @export
pull..data.frame <- function(.df, var = NULL) {

  .df <- as_tidytable(.df)

  var <- enquo(var)
  if (quo_is_null(var)) var <- sym(names(.df)[ncol(.df)])

  # Base R translation is faster than data.table
  eval_quo(
    '$'(.df, !!var)
  )
}

#' @export
#' @rdname pull.
dt_pull <- function(.df, var = NULL) {
  deprecate_soft("0.5.2", "tidytable::dt_pull()", "pull.()")

  pull.(.df, var = {{ var }})
}
