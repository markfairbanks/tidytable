#' Count observations by group
#'
#' @description
#' Returns row counts of the dataset. If bare column names are provided, `dt_count()` returns counts by group.
#'
#' Supports enhanced selection
#'
#' @param .data A data.frame or data.table
#' @param ... Optional: Bare column names to group by
#'
#' @return A data.table
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
#'   dt_count()
#'
#' example_df %>%
#'   dt_count(z)
#'
#' example_df %>%
#'   dt_count(is.character)
dt_count <- function(.data, ...) {
  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  dots <- enexprs(...)

  if (length(dots) == 0) {
    .data[, list(N = .N), .N]
  } else {
    by_vec <- dots_selector(.data, ...) %>%
      as.character()

    eval_tidy(expr(
      .data[, .N, by = by_vec]
    ))
  }
}
