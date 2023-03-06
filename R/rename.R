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
  .df <- .df_as_tidytable(.df)

  if (is_ungrouped(.df)) {
    tt_rename(.df, ...)
  } else {
    # Ensure "groups" attribute has new names
    groups <- group_vars(.df)
    groups <- select(.df, all_of(groups))
    groups <- names(tt_rename(groups, ..., .strict = FALSE))

    out <- ungroup(.df)
    out <- tt_rename(out, ...)

    group_by(out, all_of(groups))
  }
}

#' @export
#' @keywords internal
#' @inherit rename
rename. <- function(.df, ...) {
  deprecate_dot_fun()
  rename(.df, ...)
}

tt_rename <- function(.df, ..., .strict = TRUE) {
  locs <- eval_rename(expr(c(...)), .df, strict = .strict)

  names <- names(.df)
  names[locs] <- names(locs)

  set_names(.df, names)
}

