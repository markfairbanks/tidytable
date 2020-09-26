#' Select or drop columns
#'
#' @description
#' Select or drop columns from a data.table
#'
#' @param .df A data.frame or data.table
#' @param ... Columns to select or drop.
#' Use named arguments, e.g. new_name = old_name, to rename selected variables.
#' `tidyselect` compatible.
#'
#' @export
#' @md
#'
#' @examples
#' test_df <- data.table(
#'   x = c(1,1,1),
#'   y = c(4,5,6),
#'   double_x = c(2,2,2),
#'   z = c("a","a","b"))
#'
#' test_df %>%
#'   select.(x, y)
#'
#' test_df %>%
#'   select.(x:z)
#'
#' test_df %>%
#'   select.(-y, -z)
#'
#' test_df %>%
#'   select.(starts_with("x"), z)
#'
#' test_df %>%
#'   select.(where(is.character), x)
#'
#' test_df %>%
#'   select.(stuff = x, y)
select. <- function(.df, ...) {
  UseMethod("select.")
}

#' @export
select..data.frame <- function(.df, ...) {

  .df <- as_tidytable(.df)

  select_cols <- select_dots_idx(.df, ...)

  .df <- eval_quo(.df[, !!select_cols])

  .df <- set_names(.df, names(select_cols))

  .df
}

#' @export
#' @rdname dt_verb
#' @inheritParams select.
dt_select <- function(.df, ...) {
  deprecate_stop("0.5.2", "tidytable::dt_select()", "select.()")

  select.(.df, ...)
}
