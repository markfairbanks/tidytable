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

  select_cols <- select_dots_chr(.df, ...)

  subset_data <- .df[, ..select_cols]

  numeric_flag <- map_lgl.(subset_data, is.numeric)

  with_by <- !quo_is_null(.by)

  if (with_by) col_order <- names(.df)

  select_cols <- syms(select_cols)

  if (all(numeric_flag)) {

    # Use data.table::nafill() if all numeric
    if (.direction %in% c("down", "up")) {
      .type <- switch(.direction, "down" = "locf", "up" = "nocb")

      .df <- mutate_across.(
        .df, c(!!!select_cols), nafill, .type, .by = !!.by
      )
    } else if (.direction == "downup") {
      .df <- mutate_across.(
        .df, c(!!!select_cols),
        ~ nafill(nafill(.x, type = "locf"), type = "nocb"),
        .by = !!.by
      )
    } else {
      .df <- mutate_across.(
        .df, c(!!!select_cols),
        ~ nafill(nafill(.x, type = "nocb"), type = "locf"),
        .by = !!.by
      )
    }

  } else {

    # Use vctrs::vec_fill_missing() if there are any character cols
    .df <- mutate_across.(
      .df, c(!!!select_cols), vec_fill_missing, direction = .direction, .by = !!.by
    )
  }

  if (with_by) setcolorder(.df, col_order)

  .df
}
