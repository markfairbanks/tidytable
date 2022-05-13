#' Bind data.tables by row and column
#'
#' @description
#' Bind multiple data.tables into one row-wise or col-wise.
#'
#' @param ... data.tables or data.frames to bind
#' @param .id If TRUE, an integer column is made as a group id
#' @param .name_repair Treatment of duplicate names. See `?vctrs::vec_as_names` for options/details.
#'
#' @export
#' @md
#'
#' @examples
#'
#' # Binding data together by row
#' df1 <- data.table(x = 1:3, y = 10:12)
#' df2 <- data.table(x = 4:6, y = 13:15)
#'
#' df1 %>%
#'   bind_rows.(df2)
#'
#' # Can pass a list of data.tables
#' df_list <- list(df1, df2)
#'
#' bind_rows.(df_list)
#'
#' # Binding data together by column
#' df1 <- data.table(a = 1:3, b = 4:6)
#' df2 <- data.table(c = 7:9)
#'
#' df1 %>%
#'   bind_cols.(df2)
#'
#' # Can pass a list of data frames
#' bind_cols.(list(df1, df2))
#' @export
bind_cols. <- function(..., .name_repair = "unique") {
  dots <- list2(...)
  dots <- squash(dots)

  out <- vec_cbind(!!!dots, .ptype = tidytable(), .name_repair = .name_repair)

  first <- dots[[1]]

  if (is_tidytable(first)) {
    out <- tidytable_restore(out, first)
  }

  out
}

#' @export
#' @rdname bind_cols.
bind_rows. <- function(..., .id = NULL) {
  dots <- list2(...)
  dots <- squash(dots)

  out <- as_tidytable(rbindlist(dots, idcol = .id, use.names = TRUE, fill = TRUE))

  first <- dots[[1]]

  if (is_tidytable(first)) {
    out <- tidytable_restore(out, first)
  }

  out
}

