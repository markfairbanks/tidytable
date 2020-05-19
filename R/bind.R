#' Bind data.tables by row and column
#'
#' @description
#' Bind multiple data.tables into one row-wise or col-wise.
#'
#' @param ... data.tables or data.frames to bind
#' @param .id If TRUE, an integer column is made as a group id
#' @param use.names If TRUE, makes sure column names align
#' @param fill If TRUE, fills missing columns with NA
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
bind_rows. <- function(..., .id = NULL, use.names = TRUE, fill = TRUE) {

  dots <- list(...)
  dots <- squash(dots)

  if (!all(map_lgl.(dots, is.data.table)))
    dots <- map.(dots, as_tidytable)

  dots <- rbindlist(dots, idcol = .id, use.names = use.names, fill = fill)

  as_tidytable(dots)
}

#' @export
#' @rdname bind_rows.
dt_bind_rows <- bind_rows.

#' @export
#' @rdname bind_rows.
bind_cols. <- function(...) {

  dots <- list(...)
  dots <- squash(dots)

  if (!all(map_lgl.(dots, is.data.table)))
    dots <- map.(dots, as_tidytable)

  as_tidytable(name_fix(setDT(unlist(dots, recursive = FALSE), check.names = FALSE)[]))

}

#' @export
#' @rdname bind_rows.
dt_bind_cols <- bind_cols.

name_fix <- function(.data) {

  col_names <- names(.data)

  dupe_count <- map_dbl.(seq_along(col_names), ~ sum(col_names[.x] == col_names[1:.x]))

  col_names[dupe_count > 1] <- paste0(
    col_names[dupe_count > 1],
    dupe_count[dupe_count > 1] - 1
  )

  names(.data) <- col_names

  .data
}
