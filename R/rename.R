#' Rename variables by name
#'
#' @description
#' Rename variables from a data.table.
#'
#' @param .df A data.frame or data.table
#' @param ... Rename expression like dplyr::rename()
#'
#' @export
#'
#' @examples
#' dt <- data.table(x = c(1,2,3), y = c(4,5,6))
#'
#' dt %>%
#'   rename.(new_x = x,
#'           new_y = y)
rename. <- function(.df, ...) {
  UseMethod("rename.")
}

#' @export
rename..data.frame <- function(.df, ...) {

  .df <- as_tidytable(.df)

  dots <- enquos(...)
  .df <- shallow(.df)

  new_names <- names(dots)
  old_names <- map_chr.(dots, quo_name)

  setnames(.df, old_names, new_names)

  .df
}

#' @export
#' @rdname dt_verb
#' @inheritParams rename.
dt_rename <- function(.df, ...) {
  deprecate_warn("0.5.2", "tidytable::dt_rename()", "rename.()")

  rename.(.df, ...)
}
