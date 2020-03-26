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
#'   bind_rows.(df2)
#'
#' bind_rows.(list(df1, df2))
#'
#' df1 %>%
#'   bind_cols.(df2)
#'
#' bind_cols.(list(df1, df2))
bind_rows. <- function(..., .id = NULL) {
  UseMethod("bind_rows.")
}

#' @export
bind_rows..default <- function(..., .id = NULL) {

  dots <- list(...)
  dots <- squash(dots)

  if (!all(dt_map_lgl(dots, is.data.table)))
    dots <- dt_map(dots, as_tidytable)

  dots <- rbindlist(dots, idcol = .id)

  as_tidytable(dots)
}

#' @export
#' @rdname bind_rows.
dt_bind_rows <- bind_rows.

#' @export
#' @rdname bind_rows.
bind_cols. <- function(...) {
  UseMethod("bind_cols.")
}

#' @export
bind_cols..default <- function(...) {

  dots <- list(...)
  dots <- squash(dots)

  if (!all(dt_map_lgl(dots, is.data.table)))
    dots <- dt_map(dots, as_tidytable)

  as_tidytable(name_fix(setDT(unlist(dots, recursive = FALSE), check.names = FALSE)[]))

}

#' @export
#' @rdname bind_rows.
dt_bind_cols <- bind_cols.

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
