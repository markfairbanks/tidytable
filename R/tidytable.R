#' Build a data.table/tidytable
#'
#' @description
#' `tidytable()` constructs a data.table, but one with nice printing features.
#'
#' @param ... Arguments passed to `data.table()`
#'
#' @md
#' @export
#'
#' @examples
#' tidytable(x = 1:3, y = c("a", "a", "b"))
tidytable <- function(...) {
  dots <- enquos(...)

  mask <- build_data_mask(dots)

  dt_expr <- call2_dt("data.table", !!!dots)

  .df <- eval_tidy(dt_expr, mask, caller_env())

  as_tidytable(.df)
}
