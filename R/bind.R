#' Bind data.tables by row and column
#'
#' @description
#' Bind multiple data.tables into one row-wise or col-wise.
#'
#' @param ... data.tables or data.frames to bind
#' @param .id If TRUE, an integer column is made as a group id
#' @param .use_names If TRUE, makes sure column names align
#' @param .fill If TRUE, fills missing columns with NA
#'
#' @export
#' @md
#'
#' @examples
#' df1 <- data.table(x = c(1,2,3), y = c(3,4,5))
#' df2 <- data.table(x = c(1,2,3), y = c(3,4,5))
#'
#' df1 %>%
#'   bind_rows.(df2)
#'
#' bind_rows.(list(df1, df2))
#'
#' df1 %>%
#'   bind_cols.(df2)
#'
#' bind_cols.(list(df1, df2))
#' @export
bind_rows. <- function(..., .id = NULL, .use_names = TRUE, .fill = TRUE) {

  dots <- list(...)
  dots <- squash(dots)

  if (!all(map_lgl.(dots, is.data.table)))
    dots <- map.(dots, as_tidytable)

  dots <- rbindlist(dots, idcol = .id, use.names = .use_names, fill = .fill)

  as_tidytable(dots)
}

#' @export
#' @rdname bind_rows.
dt_bind_rows <- function(..., .id = NULL, .use_names = TRUE, .fill = TRUE) {
  deprecate_soft("0.5.2", "tidytable::dt_bind_rows()", "bind_rows.()")

  bind_rows.(..., .id = .id, .use_names = .use_names, .fill = .fill)
}

#' @export
#' @rdname bind_rows.
bind_cols. <- function(...) {

  dots <- list(...)
  dots <- squash(dots)

  if (!all(map_lgl.(dots, is.data.table)))
    dots <- map.(dots, as_tidytable)

  dots <- setDT(unlist(dots, recursive = FALSE), check.names = FALSE)

  names(dots) <- vec_as_names_legacy(names(dots))

  as_tidytable(dots)

}

#' @export
#' @rdname bind_rows.
dt_bind_cols <- function(...) {
  deprecate_soft("0.5.2", "tidytable::dt_bind_cols()", "bind_cols.()")

  bind_cols.(...)
}

