#' Read/write files
#'
#' @description
#' `fread()` is a simple wrapper around `data.table::fread()` that returns a tidytable
#' instead of a data.table.
#'
#' @param ... Arguments passed on to `data.table::fread`
#'
#' @export
#'
#' @examples
#' fake_csv <- "A,B
#'              1,2
#'              3,4"
#'
#' fread(fake_csv)
fread <- function(...) {
  .df <- data.table::fread(...)
  as_tidytable(.df)
}
