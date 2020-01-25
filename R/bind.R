#' Bind data.tables by row and column
#'
#' @description
#'
#' Bind rows or columns together.
#'
#'
#' @param .data A data.table to bind, or a list of data.tables
#' @param ... Data frames to bind
#' @param .id If TRUE, an integer column is made as a group id
#'
#'
#' @import data.table
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
dt_bind_rows <- function(.data, ..., .id = NULL) {
  # check if input .data is already a list; if not, transform to list
  if (class(.data)[1] != "list") {
    .data <- list(.data)
  }

  dots <- list(...)
  dots <- append(.data, dots)

  if (!all(dt_map_lgl(dots, is.data.frame)))
    stop("All inputs must be a data.frame or data.table")

  if (!all(dt_map_lgl(dots, is.data.table)))
    dots <- dt_map(dots, as.data.table)

  rbindlist(dots, idcol = .id)
}

#' @export
#' @rdname dt_bind_rows
dt_bind_cols <- function(.data, ...) {
  # check if input .data is already a list; if not, transform to list
  if (class(.data)[1] != "list") {
    .data <- list(.data)
  }

  dots <- list(...)
  dots <- append(.data, dots)

  if (!all(dt_map_lgl(dots, is.data.frame)))
    stop("All inputs must be a data.frame or data.table")

  if (!all(dt_map_lgl(dots, is.data.table)))
    dots <- dt_map(dots, as.data.table)

  result_df <- name_fix(do.call(cbind, dots))

  result_df
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
