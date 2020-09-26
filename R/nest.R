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
#' test_df <- data.table(
#'   a = 1:10,
#'   b = 11:20,
#'   c = c(rep("a", 6), rep("b", 4)),
#'   d = c(rep("a", 4), rep("b", 6)))
#'
#' test_df %>%
#'   nest_by.()
#'
#' test_df %>%
#'   nest_by.(c, d)
#'
#' test_df %>%
#'   nest_by.(where(is.character))
#'
#' test_df %>%
#'   nest_by.(c, d, .keep = TRUE)
nest_by. <- function(.df, ..., .key = "data", .keep = FALSE) {
  UseMethod("nest_by.")
}

#' @export
nest_by..data.frame <- function(.df, ..., .key = "data", .keep = FALSE) {

  .df <- as_tidytable(.df)

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
#' @rdname dt_verb
#' @inheritParams nest_by.
dt_group_nest <- function(.df, ..., .key = "data", .keep = FALSE) {
  deprecate_stop("0.5.2", "tidytable::dt_group_nest()", "nest_by.()")

  nest_by.(.df, ..., .key = .key, .keep = .keep)
}
