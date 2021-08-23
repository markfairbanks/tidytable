#' Read/write files
#'
#' @description
#' `fread.()` is a simple wrapper around `data.table::fread()` that returns a tidytable
#' instead of a data.table.
#'
#' `fwrite.()` is a simple wrapper around `data.table::fwrite()`.
#'
#' @param ... Arguments passed on to `data.table::fread` or `data.table::fwrite`.
#'     See `?data.table::fread` or `?data.table::fwrite` for details
#'
#' @export
#'
#' @examples
#' fake_csv <- "A,B
#'              1,2
#'              3,4"
#'
#' fread.(fake_csv)
fread. <- function(...) {
  .df <- fread(...)
  as_tidytable(.df)
}

#' @export
#' @rdname fread.
fwrite. <- function(...) {
  fwrite(...)
}
