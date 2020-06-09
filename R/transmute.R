#' Add new variables and drop all others
#'
#' @description
#' Unlike `mutate.()`, `transmute.()` keeps only the variables that you create
#'
#' @param .df A data.frame or data.table
#' @param ... Columns to create/modify
#' @param .by Columns to group by
#' @param by This argument has been renamed to .by and is deprecated
#'
#' @md
#' @examples
#' mtcars %>%
#'   transmute.(displ_l = disp / 61.0237)
#'
#' @export
transmute. <- function(.df, ..., .by = NULL, by = NULL) {
  UseMethod("transmute.")
}

#' @export
transmute..data.frame <- function(.df, ..., .by = NULL, by = NULL) {

  .df <- as_tidytable(.df)

  dots <- enquos(...)
  keep_names <- names(dots)

  .by <- check_dot_by(enquo(.by), enquo(by), "transmute.")

  .df <- mutate.(.df, ..., .by = !!.by)

  .df[, ..keep_names]
}

#' @export
#' @rdname transmute.
dt_transmute <- function(.df, ..., .by = NULL, by = NULL) {
  deprecate_soft("0.5.2", "tidytable::dt_transmute()", "transmute.()")

  .by <- check_dot_by(enquo(.by), enquo(by))

  transmute.(.df, ..., .by = !!.by)
}


