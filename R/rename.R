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
#' dt <- data.table::data.table(x = c(1,2,3), y = c(4,5,6))
#'
#' dt %>%
#'   dt_rename(new_x = x,
#'             new_y = y)
dt_rename <- function(.data, ...) {

  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  dots <- enexprs(...)

  new_names <- names(dots)
  old_names <- as.character(dots)

  setnames(.data, old_names, new_names)

  .data
}
