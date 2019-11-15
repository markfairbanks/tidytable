#' Run invisible garbage collection
#'
#' Run garbage collection without the gc() output. Can also be run in the middle of a long pipe chain. Useful for large datasets.
#'
#' @return
#' @export
#'
#' @examples
#' inv_gc()
#'
#' df %>%
#'   inv_gc() %>%
#'   select(col1, col2)
inv_gc <- function(df = NULL) {
  if(is.null(df)) {
    invisible(gc())
  } else {
    invisible(gc())
    return(df)
  }
}
