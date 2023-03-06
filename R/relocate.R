#' Relocate a column to a new position
#'
#' @description
#' Move a column or columns to a new position
#'
#' @param .df A data.frame or data.table
#' @param ... A selection of columns to move. `tidyselect` compatible.
#' @param .before Column to move selection before
#' @param .after Column to move selection after
#'
#' @export
#'
#' @examples
#' df <- data.table(
#'   a = 1:3,
#'   b = 1:3,
#'   c = c("a", "a", "b"),
#'   d = c("a", "a", "b")
#' )
#'
#' df %>%
#'   relocate(c, .before = b)
#'
#' df %>%
#'   relocate(a, b, .after = c)
#'
#' df %>%
#'   relocate(where(is.numeric), .after = c)
relocate <- function(.df, ..., .before = NULL, .after = NULL) {
  .df <- .df_as_tidytable(.df)

  order <- eval_relocate(
    expr(c(...)),
    .df,
    before = enquo(.before),
    after = enquo(.after),
    before_arg = ".before",
    after_arg = ".after"
  )

  names <- names(order)

  out <- df_col_order(.df, order)

  df_set_names(out, names)
}

#' @export
#' @keywords internal
#' @inherit relocate
relocate. <- function(.df, ..., .before = NULL, .after = NULL) {
  deprecate_dot_fun()
  relocate(.df, ..., .before = {{ .before }}, .after = {{ .after }})
}


