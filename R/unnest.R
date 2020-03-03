#' Unnest a nested data.table
#'
#' @description
#' Unnest a nested data.table.
#'
#' @param .data A nested data.table
#' @param col The column to unnest
#'
#' @export
#'
#' @examples
#' nested_df <- data.table::data.table(
#'   a = 1:10,
#'   b = 11:20,
#'   c = c(rep("a", 6), rep("b", 4)),
#'   d = c(rep("a", 4), rep("b", 6))) %>%
#'   dt_group_nest(c, d)
#'
#' nested_df %>%
#'   dt_unnest_legacy(data)
dt_unnest_legacy <- function(.data, col = NULL) {
  UseMethod("dt_unnest_legacy")
}

#' @export
dt_unnest_legacy.data.frame <- function(.data, col = NULL) {
  if (!is_tidytable(.data)) .data <- as_tidytable(.data)

  col <- enexpr(col)

  if (is.null(col)) abort("col must be supplied")

  keep_cols <- colnames(.data)[!dt_map_lgl(.data, is.list)]

  is_datatable <- is.data.table(.data[[col]][[1]])

  if (is_datatable) {
    .data <- eval_tidy(expr(
      .data[, unlist(!!col, recursive = FALSE), by = keep_cols]
    ))
  } else {
    .data <- eval_tidy(expr(
      .data[, list(.new_col = unlist(!!col, recursive = FALSE)), by = keep_cols]
    )) %>%
      dt_rename(!!col := .new_col)
  }
  .data
}
