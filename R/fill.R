#' Fill in missing values with previous or next value
#'
#' @description
#' Fills missing values in the selected columns using the next or previous entry. Can be done by group.
#'
#' Supports tidyselect
#'
#' @param .df A data.frame or data.table
#' @param ... A selection of columns. `tidyselect` compatible.
#' @param .direction Direction in which to fill missing values.
#' Currently "down" (the default), "up", "downup" (first down then up), or "updown" (first up and then down)
#' @param .by Columns to group by when filling should be done by group
#'
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
#'   fill.(x, y, .by = z)
#'
#' test_df %>%
#'   fill.(x, y, .by = z, .direction = "downup")
fill. <- function(.df, ...,
                  .direction = c("down", "up", "downup", "updown"),
                  .by = NULL) {
  UseMethod("fill.")
}

#' @export
fill..data.frame <- function(.df, ...,
                             .direction = c("down", "up", "downup", "updown"),
                             .by = NULL) {

  .df <- as_tidytable(.df)

  .by <- enquo(.by)

  .direction <- arg_match(.direction)

  if (.direction == "down") {
    filler(.df, ..., .type = "down", .by = !!.by)
  } else if (.direction == "up") {
    filler(.df, ..., .type = "up", .by = !!.by)
  } else if (.direction == "downup") {
    .df %>%
      filler(..., .type = "down", .by = !!.by) %>%
      filler(..., .type = "up", .by = !!.by)
  } else {
    .df %>%
      filler(..., .type = "up", .by = !!.by) %>%
      filler(..., .type = "down", .by = !!.by)
  }
}

#' @export
#' @rdname dt_verb
#' @inheritParams fill.
dt_fill <- function(.df, ...,
                    .direction = c("down", "up", "downup", "updown"),
                    .by = NULL) {

  deprecate_stop("0.5.2", "tidytable::dt_fill()", "fill.()")

  fill.(.df, ..., .direction = .direction, .by = {{ .by }})
}

filler <- function(.df, ..., .type = "down", .by = NULL) {

  .type <- switch(.type, "down" = "locf", "up" = "nocb")

  all_cols <- select_dots_chr(.df, ...)

  .by <- enquo(.by)

  subset_data <- .df[, ..all_cols]

  numeric_flag <- map_lgl.(subset_data, is.numeric)
  numeric_cols <- all_cols[numeric_flag]
  other_cols <- all_cols[!numeric_flag]

  with_by <- !quo_is_null(.by)

  if (with_by) col_order <- names(.df)

  if (length(numeric_cols) > 0) {

    numeric_cols <- syms(numeric_cols)

    .df <- mutate_across.(.df,
                          c(!!!numeric_cols),
                          nafill, .type,
                          .by = !!.by)

  }

  if (length(other_cols) > 0) {

    other_cols <- syms(other_cols)
    .df <- shallow(.df)

    .by <- select_vec_chr(.df, !!.by)

    for (col in other_cols) {
      eval_quo(
        .df[, !!col := .SD[, !!col][nafill(fifelse(is.na(!!col), NA_integer_, 1:.N), type = !!.type)],
          by = .by]
      )
    }
  }

  if (with_by) setcolorder(.df, col_order)

  .df
}
