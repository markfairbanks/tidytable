#' Nest data.tables
#'
#' @description
#' Nest data.tables by group.
#'
#' Note: `nest_by()` _does not_ return a rowwise tidytable.
#'
#' @param .df A data.frame or data.table
#' @param ... Columns to group by. If empty nests the entire data.table.
#' `tidyselect` compatible.
#' @param .key Name of the new column created by nesting.
#' @param .keep Should the grouping columns be kept in the list column.
#'
#' @export
#'
#' @examples
#' df <- data.table(
#'   a = 1:5,
#'   b = 6:10,
#'   c = c(rep("a", 3), rep("b", 2)),
#'   d = c(rep("a", 3), rep("b", 2))
#' )
#'
#' df %>%
#'   nest_by()
#'
#' df %>%
#'   nest_by(c, d)
#'
#' df %>%
#'   nest_by(where(is.character))
#'
#' df %>%
#'   nest_by(c, d, .keep = TRUE)
nest_by <- function(.df, ..., .key = "data", .keep = FALSE) {
  UseMethod("nest_by")
}

#' @export
nest_by.tidytable <- function(.df, ..., .key = "data", .keep = FALSE) {
  if (is_true(.keep)) {
    split_list <- group_split(.df, ..., .keep = .keep)

    .df <- distinct(.df, ...)

    .df <- mutate(.df, !!.key := .env$split_list)
  } else {
    .df <- summarize(.df, !!.key := list(.SD), .by = c(...))
  }

  .df
}

#' @export
nest_by.grouped_tt <- function(.df, ..., .key = "data", .keep = FALSE) {
  .by <- group_vars(.df)
  out <- ungroup(.df)
  out <- nest_by(out, any_of(.by), .key = .key, .keep = .keep)
  group_by(out, any_of(.by))
}

#' @export
nest_by.data.frame <- function(.df, ..., .key = "data", .keep = FALSE) {
  .df <- as_tidytable(.df)
  nest_by(.df, ..., .key = .key, .keep = .keep)
}

