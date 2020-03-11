#' Fill in missing values with previous or next value
#'
#' @description
#' Fills missing values in the selected columns using the next or previous entry. Can be done by group.
#'
#' @param .data A data.frame or data.table
#' @param ... A selection of bare columns
#' @param .direction Direction in which to fill missing values. Currently "down" (the default), "up", "downup" (first down then up), or "updown" (first up and then down)
#' @param by Whether the filling should be done by group. Passed in a `list()`
#' @export
#' @md
#'
#' @examples
#' test_df <- data.table::data.table(
#'   x = c(NA, NA, NA, 4:10),
#'   y = c(1:6, NA, 8, NA, 10),
#'   z = c(rep("a", 8), rep("b", 2)))
#'
#' test_df %>%
#'   dt_fill(x, y, by = z)
#'
#' test_df %>%
#'   dt_fill(x, y, by = z, .direction = "downup")
dt_fill <- function(.data, ..., .direction = c("down", "up", "downup", "updown"), by = NULL) {
  UseMethod("dt_fill")
}

#' @export
dt_fill.tidytable <- function(.data, ..., .direction = c("down", "up", "downup", "updown"), by = NULL) {
  by <- enexpr(by)

  if (length(.direction) > 1) .direction <- "down"

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
dt_fill.data.frame <- function(.data, ..., .direction = c("down", "up", "downup", "updown"), by = NULL) {
  .data <- as_tidytable(.data)
  by <- enexpr(by)

  dt_fill(.data, ..., .direction = .direction, by = !!by)

}

filler <- function(.data, ..., type = "locf", by = NULL) {

  all_cols <- as.character(dots_selector(.data, ...))
  by <- enexpr(by)

  subset_data <- .data[, ..all_cols]

  numeric_cols <- all_cols[dt_map_lgl(subset_data, is.numeric)]
  other_cols <- all_cols[!all_cols %in% numeric_cols]

  if (length(numeric_cols) > 0)
    .data <- eval_expr(
      .data %>%
        dt( , !!numeric_cols := lapply(.SD, nafill, !!type), .SDcols = !!numeric_cols, by = !!by)
    )
  if (length(other_cols) > 0) {
    other_cols <- syms(other_cols)

    for (col in other_cols) {
      .data <- eval_expr(
        .data %>%
          dt(, !!col := .SD[, !!col][nafill(fifelse(is.na(!!col), NA_integer_, 1:.N), type = !!type)],
             by = !!by)
      )
    }
  }
  .data
}
