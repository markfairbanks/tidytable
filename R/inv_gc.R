#' Run invisible garbage collection
#'
#' @description
#' Run garbage collection without the `gc()` output. Can also be run in the middle of a long pipe chain. Useful for large datasets.
#'
#' @param .data Optional. If null runs `gc()` silently. Else returns the same object unaltered.
#' @export
#'
#' @examples
#' inv_gc()
#'
#' df <- data.table::data.table(col1 = 1, col2 = 2)
#'
#' df %>%
#'   inv_gc() %>%
#'   dt_select(col1, col2)
inv_gc <- function(.data = NULL) {
  if(missing(.data)) {
    invisible(gc())
  } else {
    invisible(gc())
    return(.data)
  }
}
