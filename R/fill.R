#' Fill in missing values with previous or next value
#'
#' @description
#' Fills missing values in the selected columns using the next or previous entry. Can be done by group.
#'
#' Supports tidyselect
#'
#' @param .data A data.frame or data.table
#' @param ... A selection of columns. `tidyselect` compatible.
#' @param .direction Direction in which to fill missing values. Currently "down" (the default), "up", "downup" (first down then up), or "updown" (first up and then down)
#' @param by Columns to group by when filling should be done by group
#' @export
#' @md
#'
#' @examples
#' test_df <- tidytable(
#'   x = c(NA, NA, NA, 4:10),
#'   y = c(1:6, NA, 8, NA, 10),
#'   z = c(rep("a", 8), rep("b", 2)))
#'
#' test_df %>%
#'   fill.(x, y, by = z)
#'
#' test_df %>%
#'   fill.(x, y, by = z, .direction = "downup")
fill. <- function(.data, ..., .direction = c("down", "up", "downup", "updown"), by = NULL) {
  UseMethod("fill.")
}

#' @export
fill..data.frame <- function(.data, ..., .direction = c("down", "up", "downup", "updown"), by = NULL) {

  .data <- as_tidytable(.data)

  by <- enexpr(by)

  if (length(.direction) > 1) .direction <- .direction[1]

  if (.direction == "down") {
    filler(.data, ..., type = "locf", by = !!by)
  } else if (.direction == "up") {
    filler(.data, ..., type = "nocb", by = !!by)
  } else if (.direction == "downup") {
    .data %>%
      filler(..., type = "locf", by = !!by) %>%
      filler(..., type = "nocb", by = !!by)
  } else {
    .data %>%
      filler(..., type = "nocb", by = !!by) %>%
      filler(..., type = "locf", by = !!by)
  }
}

#' @export
#' @rdname fill.
dt_fill <- fill.

filler <- function(.data, ..., type = "locf", by = NULL) {

  all_cols <- as.character(dots_selector(.data, ...))

  by <- enexpr(by)
  by <- vec_selector_by(.data, !!by)

  subset_data <- .data[, ..all_cols]

  numeric_cols <- all_cols[map_lgl.(subset_data, is.numeric)]
  other_cols <- all_cols[!all_cols %in% numeric_cols]

  with_by <- !is.null(by)

  if (with_by) col_order <- names(.data)

  if (length(numeric_cols) > 0) {
    .data <- shallow(.data)

    eval_expr(
      .data[, !!numeric_cols := lapply(.SD, nafill, !!type), .SDcols = !!numeric_cols, by = !!by]
    )
  }
  if (length(other_cols) > 0) {
    other_cols <- syms(other_cols)
    .data <- shallow(.data)

    for (col in other_cols) {
      eval_expr(
        .data[, !!col := .SD[, !!col][nafill(fifelse(is.na(!!col), NA_integer_, 1:.N), type = !!type)],
              by = !!by]
      )
    }
  }

  if (with_by) setcolorder(.data, col_order)

  .data
}
