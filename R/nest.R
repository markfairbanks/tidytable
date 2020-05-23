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

  if (.keep) {

    split_vars <- select_dots_sym(.df, ...)

    split_list <- group_split.(.df, !!!split_vars, .keep = .keep)

    .df <- distinct.(.df, !!!split_vars)

    .df <- mutate.(.df, {{.key}} := split_list)

  } else {

    by <- enquos(...)

    .df <- summarize.(.df, {{.key}} := list(.SD), by = c(!!!by))
  }

  .df

}

#' @export
#' @rdname nest_by.
dt_group_nest <- nest_by.
