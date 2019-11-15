#' Title
#'
#' @param .data
#' @param ... Rename expression like dplyr::rename()
#'
#' @return
#' @export
#'
#' @examples
#' dt <- data.table(x = c(1,2,3), y = c(4,5,6))
#' dt %>% rename(new_x = x,
#'               new_y = y)
#'
rename <- function(.data, ...) {

  is.data.frame(.data) || is.data.table(.data) || stop(".data must be a data.table")

  if (!is.data.table(.data)) {
    .data = as.data.table(.data)
  }

  .dots <- enlist_dots(...)

  for (i in seq_along(.dots)) {
    new_name <- names(.dots)[[i]]
    old_name <- as.character(.dots[[i]])

    setnames(.data, old_name, new_name)
  }
  .data
}
