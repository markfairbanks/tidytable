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
  .df <- .df_as_tidytable(.df)

  if (is_ungrouped(.df)) {
    tt_nest_by(.df, ..., .key = .key, .keep = .keep)
  } else {
    .by <- group_vars(.df)
    tt_nest_by(.df, any_of(.by), .key = .key, .keep = .keep)
  }
}

#' @export
#' @keywords internal
#' @inherit nest_by
nest_by. <- function(.df, ..., .key = "data", .keep = FALSE) {
  deprecate_dot_fun()
  nest_by(.df, ..., .key = .key, .keep = .keep)
}

tt_nest_by <- function(.df, ..., .key = "data", .keep = FALSE) {
  if (is_true(.keep)) {
    split_list <- group_split(.df, ..., .keep = .keep)

    .df <- distinct(.df, ...)

    .df <- mutate(.df, !!.key := .env$split_list)
  } else {
    .by <- enquos(...)

    .df <- summarize(.df, !!.key := list(.SD), .by = c(!!!.by))
  }

  .df
}

