#' Add new variables and drop all others
#'
#' @description
#' Unlike `dt_mutate()`, `transmute.()` keeps only the variables that you create
#'
#' @param .data A data.frame or data.table
#' @param ... Columns to create/modify
#' @param by Columns to group by
#'
#' @md
#' @examples
#' # transmute keeps only the variables you create
#' mtcars %>%
#'   transmute.(displ_l = disp / 61.0237)
#'
#' @export
transmute. <- function(.data, ..., by = NULL) {
  UseMethod("transmute.")
}

#' @export
transmute..tidytable <- function(.data, ..., by = NULL) {

  dots <- enexprs(...)
  by <- enexpr(by)
  by <- vec_selector_by(.data, !!by)

  eval_expr(
    .data[, list(!!!dots), by = !!by]
  )
}

#' @export
transmute..data.frame <- function(.data, ..., by = NULL) {

  .data <- as_tidytable(.data)
  by <- enexpr(by)

  transmute.(.data, ..., by = !!by)
}

#' @export
#' @rdname transmute.
dt_transmute <- transmute.


