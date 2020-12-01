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
bind_cols. <- function(..., .name_repair = "unique") {

  dots <- list(...)
  dots <- squash(dots)

  if (!all(map_lgl.(dots, is.data.table)))
    dots <- map.(dots, as_tidytable)

  dots <- setDT(unlist(dots, recursive = FALSE), check.names = FALSE)

  dots <- df_name_repair(dots, .name_repair = .name_repair)

  as_tidytable(dots)

}

#' @export
#' @rdname bind_cols.
bind_rows. <- function(..., .id = NULL) {

  dots <- list(...)
  dots <- squash(dots)

  if (!all(map_lgl.(dots, is.data.table)))
    dots <- map.(dots, as_tidytable)

  dots <- rbindlist(dots, idcol = .id, use.names = TRUE, fill = TRUE)

  as_tidytable(dots)
}

