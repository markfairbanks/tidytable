#' Build a data.table/tidytable
#'
#' @description
#' `tidytable()` constructs a data.table, but one with nice printing features.
#' As such it can be used exactly like a data.table would be used.
#'
#' @param ... Arguments passed to `data.table()`
#'
#' @md
#' @export
#'
#' @examples
#' tidytable(x = c(1,2,3), y = c(4,5,6))
tidytable <-function(...) {

  dots <- enquos(...)

  .df <- eval_quo(data.table(!!!dots))

  as_tidytable(.df)
}
