#' Add new variables and drop all others
#'
#' @description
#' Unlike `mutate.()`, `transmute.()` keeps only the variables that you create
#'
#' @param .df A data.frame or data.table
#' @param ... Columns to create/modify
#' @param by Columns to group by
#'
#' @md
#' @examples
#' mtcars %>%
#'   transmute.(displ_l = disp / 61.0237)
#'
#' @export
transmute. <- function(.df, ..., by = NULL) {
  UseMethod("transmute.")
}

#' @export
transmute..data.frame <- function(.df, ..., by = NULL) {

  .df <- as_tidytable(.df)

  dots <- enquos(...)
  keep_names <- names(dots)

  .df <- mutate.(.df, ..., by = {{ by }})

  .df[, ..keep_names]
}

#' @export
#' @rdname transmute.
dt_transmute <- transmute.


