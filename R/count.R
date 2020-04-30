#' Count observations by group
#'
#' @description
#' Returns row counts of the dataset. If bare column names are provided, `count.()` returns counts by group.
#'
#' @param .data A data.frame or data.table
#' @param ... Columns to group by. `tidyselect` compatible.
#'
#' @export
#' @md
#'
#' @examples
#' example_df <- tidytable(
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
count..data.frame <- function(.data, ...) {

  .data <- as_tidytable(.data)

  by <- dots_selector_by(.data, ...)

  eval_expr(
    .data[, list(N = .N), by = !!by]
  )
}

#' @export
#' @rdname count.
dt_count <- count.

