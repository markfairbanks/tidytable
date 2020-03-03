#' Bind data.tables by row and column
#'
#' @description
#' Bind rows or columns together.
#'
#' @param ... data.tables or data.frames to bind
#' @param .id If TRUE, an integer column is made as a group id
#'
#' @export
#' @md
#'
#' @examples
#' df1 <- data.table::data.table(x = c(1,2,3), y = c(3,4,5))
#' df2 <- data.table::data.table(x = c(1,2,3), y = c(3,4,5))
#'
#' df1 %>%
#'   dt_bind_rows(df2)
#'
#' dt_bind_rows(list(df1, df2))
#'
#' df1 %>%
#'   dt_bind_cols(df2)
#'
#' dt_bind_cols(list(df1, df2))
dt_bind_rows <- function(..., .id = NULL) {
  UseMethod("dt_bind_rows")
}

#' @export
dt_bind_rows.default <- function(..., .id = NULL) {

  dots <- list(...)
  dots <- squash(dots)

  if (!all(dt_map_lgl(dots, is.data.table)))
    dots <- dt_map(dots, as.data.table)

  dots <- rbindlist(dots, idcol = .id)

  as_tidytable(dots)
}

#' @export
#' @rdname dt_bind_rows
dt_bind_cols <- function(...) {
  UseMethod("dt_bind_cols")
}

#' @export
dt_bind_cols.default <- function(...) {

  dots <- list(...)
  dots <- squash(dots)

  if (!all(dt_map_lgl(dots, is.data.table)))
    dots <- dt_map(dots, as.data.table)

  as_tidytable(name_fix(setDT(unlist(dots, recursive = FALSE), check.names = FALSE)[]))

}

name_fix <- function(.data) {

  col_names <- names(.data)

  dupe_count <- dt_map_dbl(seq_along(col_names), function(i) sum(col_names[i] == col_names[1:i]))

  col_names[dupe_count > 1] <- paste0(
    col_names[dupe_count > 1],
    dupe_count[dupe_count > 1] - 1
  )

  names(.data) <- col_names

  .data
}
