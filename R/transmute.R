#' Add new variables and drop all others
#'
#' @description
#' Unlike `mutate.()`, `transmute.()` keeps only the variables that you create
#'
#' @param .df A data.frame or data.table
#' @param ... Columns to create/modify
#' @param .by Columns to group by
#'
#' @md
#' @examples
#' mtcars %>%
#'   transmute.(displ_l = disp / 61.0237)
#'
#' @export
transmute. <- function(.df, ..., .by = NULL) {
  UseMethod("transmute.")
}

#' @export
transmute..data.frame <- function(.df, ..., .by = NULL) {

  .df <- as_tidytable(.df)

  dots <- enquos(...)

  .by <- enquo(.by)

  .df <- mutate.(.df, ..., .by = !!.by)

  by_cols <- select_vec_chr(.df, !!.by)

  keep_names <- c(by_cols, names(dots))

  .df[, ..keep_names]
}

globalVariables("..keep_names")
