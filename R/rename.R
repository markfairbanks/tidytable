#' Rename variables by name
#'
#' @description
#' Rename variables from a data.table.
#'
#' @param .df A data.frame or data.table
#' @param ... `new_name = old_name` pairs to rename columns
#'
#' @export
#'
#' @examples
#' df <- data.table(x = 1:3, y = 4:6)
#'
#' df %>%
#'   rename(new_x = x,
#'          new_y = y)
rename <- function(.df, ...) {
  rename.(.df, ...)
}

#' @export
#' @keywords internal
#' @inherit rename
rename. <- function(.df, ...) {
  UseMethod("rename.")
}

#' @export
rename..tidytable <- function(.df, ...) {
  .rename(.df, ...)
}

#' @export
rename..grouped_tt <- function(.df, ...) {
  # Ensure "groups" attribute has new names
  .groups <- group_vars(.df)
  .groups <- select(.df, all_of(.groups))
  .groups <- names(.rename(.groups, ..., .strict = FALSE))

  out <-  ungroup(.df)
  out <- .rename(out, ...)

  group_by(out, all_of(.groups))
}

#' @export
rename..data.frame <- function(.df, ...) {
  .df <- as_tidytable(.df)
  rename(.df, ...)
}

.rename <- function(.df, ..., .strict = TRUE) {
  locs <- eval_rename(expr(c(...)), .df, strict = .strict)

  names <- names(.df)
  names[locs] <- names(locs)

  set_names(.df, names)
}


