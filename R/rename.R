#' Rename variables by name
#'
#' @description
#' Rename variables from a data.table.
#'
#' @param .data A data.frame or data.table
#' @param ... Rename expression like dplyr::rename()
#'
#' @export
#'
#' @examples
#' dt <- data.table::data.table(x = c(1,2,3), y = c(4,5,6))
#'
#' dt %>%
#'   rename.(new_x = x,
#'           new_y = y)
rename. <- function(.data, ...) {
  UseMethod("rename.")
}

#' @export
rename..tidytable <- function(.data, ...) {

  dots <- enexprs(...)
  .data <- shallow(.data)

  new_names <- names(dots)
  old_names <- as.character(dots)

  setnames(.data, old_names, new_names)

  .data
}

#' @export
rename..data.frame <- function(.data, ...) {
  .data <- as_tidytable(.data)

  rename.(.data, ...)
}

#' @export
#' @rdname rename.
dt_rename <- rename.
