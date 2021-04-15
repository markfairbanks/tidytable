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
#' df <- data.table(x = 1:3, y = 4:6)
#'
#' df %>%
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
  old_names <- map_chr.(dots, as_name)

  setnames(.df, old_names, new_names)

  .df
}
