#' Count observations by group
#'
#' @description
#' Returns row counts of the dataset. If bare column names are provided, `dt_count()` returns counts by group.
#'
#' Supports enhanced selection
#'
#' @param .data A data.frame or data.table
#' @param ... Columns to group by
#'
#' @export
#' @md
#'
#' @examples
#' example_df <- data.table::data.table(
#'   x = 1:3,
#'   y = 4:6,
#'   z = c("a", "a", "b"))
#'
#' example_df %>%
#'   count.()
#'
#' example_df %>%
#'   count.(z)
#'
#' example_df %>%
#'   count.(is.character)
count. <- function(.data, ...) {
  UseMethod("count.")
}

#' @export
count..tidytable <- function(.data, ...) {

  by <- dots_selector_by(.data, ...)

  eval_expr(
    .data[, list(N = .N), by = !!by]
  )
}

#' @export
count..data.frame <- function(.data, ...) {
  .data <- as_tidytable(.data)

  count.(.data, ...)
}

#' @export
#' @rdname count.
dt_count <- count.

