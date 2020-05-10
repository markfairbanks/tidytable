#' Nest data.tables
#'
#' @description
#' Nest data.tables by group
#'
#' @param .data A data.frame or data.table
#' @param ... Columns to group by. If empty nests the entire data.table.
#' `tidyselect` compatible.
#' @param .key Name of the new column created by nesting.
#' @param .keep Should the grouping columns be kept in the list column.
#'
#' @export
#' @md
#'
#' @examples
#' test_df <- data.table::data.table(a = 1:10,
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
#'   nest_by.(is.character)
#'
#' test_df %>%
#'   nest_by.(c, d, .keep = TRUE)
nest_by. <- function(.data, ..., .key = "data", .keep = FALSE) {
  UseMethod("nest_by.")
}

#' @export
nest_by..data.frame <- function(.data, ..., .key = "data", .keep = FALSE) {

  .data <- as_tidytable(.data)
  .keep <- .keep

  dots <- enexprs(...)

  if (length(dots) == 0) {

    .data <- eval_expr(.data[, list(data = list(.SD))])

  } else if (.keep) {

    split_vars <- dots_selector(.data, ...)

    split_list <- group_split.(.data, !!!split_vars, .keep = .keep)

    .data <- distinct.(.data, !!!split_vars)

    .data <- mutate.(.data, data = !!split_list)

  } else {

    by <- dots_selector_by(.data, ...)

    .data <- eval_expr(
      .data[, list(data = list(.SD)), by = !!by]
    )
  }

  if (.key != "data") .data <- rename.(.data, !!.key := data)

  .data

}

#' @export
#' @rdname nest_by.
dt_group_nest <- nest_by.
