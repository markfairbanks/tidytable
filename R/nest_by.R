#' Nest data.tables
#'
#' @description
#' Nest data.tables by group
#'
#' @param .df A data.frame or data.table
#' @param ... Columns to group by. If empty nests the entire data.table.
#' `tidyselect` compatible.
#' @param .key Name of the new column created by nesting.
#' @param .keep Should the grouping columns be kept in the list column.
#'
#' @export
#' @md
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
#'   nest_by.()
#'
#' df %>%
#'   nest_by.(c, d)
#'
#' df %>%
#'   nest_by.(where(is.character))
#'
#' df %>%
#'   nest_by.(c, d, .keep = TRUE)
nest_by. <- function(.df, ..., .key = "data", .keep = FALSE) {
  UseMethod("nest_by.")
}

#' @export
nest_by..tidytable <- function(.df, ..., .key = "data", .keep = FALSE) {
  vec_assert(.key, character(), 1)
  vec_assert(.keep, logical(), 1)

  if (.keep) {
    split_list <- group_split.(.df, ..., .keep = .keep)

    .df <- distinct.(.df, ...)

    .df <- mutate.(.df, !!.key := split_list)
  } else {
    .by <- enquos(...)

    .df <- summarize.(.df, !!.key := list(.SD), .by = c(!!!.by))
  }

  .df
}

#' @export
nest_by..data.frame <- function(.df, ..., .key = "data", .keep = FALSE) {
  .df <- as_tidytable(.df)
  nest_by.(.df, ..., .key = .key, .keep = .keep)
}
