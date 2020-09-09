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
tidytable <- function(...) {

  dots <- enquos(...)

  if (length(dots) == 0) data_env <- caller_env()
  else data_env <- env(quo_get_env(dots[[1]]))

  .df <- eval_quo(
    data.table::data.table(!!!dots),
    new_data_mask(data_env), env = caller_env()
  )

  as_tidytable(.df)
}
