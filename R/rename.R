#' Rename variables by name
#'
#' @description
#' Rename variables from a data.table.
#'
#' @param .data A data.frame or data.table
#' @param ... Rename expression like dplyr::rename()
#'
#' @return A data.table
#' @export
#'
#' @examples
#' dt <- data.table(x = c(1,2,3), y = c(4,5,6))
#' dt %>%
#'   rename(new_x = x,
#'          new_y = y)
#'
dt_rename <- function(.data, ...) {

  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  dots <- enexprs(...)
  dot_names <- names(dots)

  for (i in seq_along(dots)) {
    new_name <- dot_names[[i]]
    old_name <- as.character(dots[[i]])

    setnames(.data, old_name, new_name)
  }
  .data
}
