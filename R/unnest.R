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
#'   nest_by.(c, d)
#'
#' nested_df %>%
#'   unnest.(data)
unnest. <- function(.data, col = NULL) {
  UseMethod("unnest.")
}

#' @export
unnest..data.frame <- function(.data, col = NULL) {
  if (!is_tidytable(.data)) .data <- as_tidytable(.data)

  col <- enexpr(col)

  if (is.null(col)) abort("col must be supplied")

  keep_cols <- names(.data)[!map_lgl.(.data, is.list)]

  nested_data <- pull.(.data, !!col)[[1]]

  is_datatable <- is.data.table(nested_data)
  is_dataframe <- is.data.frame(nested_data)

  if (is_dataframe) {

    if (!is_datatable) eval_expr(shallow(.data)[, !!col := map.(!!col, as_tidytable)])

    .data <- eval_expr(
      .data[, unlist(!!col, recursive = FALSE), by = keep_cols]
    )
  } else {
    # Unnests a vector
    .data <- eval_expr(
      .data[, list(.new_col = unlist(!!col, recursive = FALSE)), by = keep_cols]
    ) %>%
      rename.(!!col := .new_col)
  }
  .data
}

#' @export
#' @rdname unnest.
dt_unnest_legacy <- unnest.

