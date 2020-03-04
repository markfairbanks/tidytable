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
  UseMethod("dt_rename")
}

#' @export
dt_rename.tidytable <- function(.data, ...) {

  dots <- enexprs(...)
  .data <- shallow(.data)

  new_names <- names(dots)
  old_names <- as.character(dots)

  setnames(.data, old_names, new_names)

  .data
}

#' @export
dt_rename.data.frame <- function(.data, ...) {
  .data <- as_tidytable(.data)

  dt_rename(.data, ...)
}
