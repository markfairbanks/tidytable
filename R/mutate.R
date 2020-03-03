#' Mutate
#'
#' @description
#' Add new columns or modify existing ones
#'
#' @param .data A data.frame or data.table
#' @param ... Columns to add/modify
#' @param by Optional: `list()` of bare column names to group by
#'
#' @md
#' @export
#'
#' @examples
#' example_dt <- data.table::data.table(
#'   a = c(1,2,3),
#'   b = c(4,5,6),
#'   c = c("a","a","b"))
#'
#' example_dt %>%
#'   dt_mutate(double_a = a * 2,
#'             a_plus_b = a + b)
#'
#' example_dt %>%
#'   dt_mutate(double_a = a * 2,
#'             avg_a = mean(a),
#'             by = c)
dt_mutate <- function(.data, ..., by = NULL) {
  UseMethod("dt_mutate")
}

#' @export
dt_mutate.tidytable <- function(.data, ..., by = NULL) {

  dots <- enexprs(...)
  by <- enexpr(by)
  .data <- shallow(.data)

  all_names <- names(dots)

  if (is.null(by)) {
    # Faster version if there is no "by" provided
    for (i in seq_along(dots)) {
      eval_tidy(expr(
        .data[, ':='(all_names[[i]], !!dots[[i]])][]
      ))
    }
  } else {
    # Faster with "by", since the "by" call isn't looped multiple times for each column added
    eval_tidy(expr(
      .data[, ':='(!!!dots), by = !!by][]
    ))
  }
  .data
}

#' @export
dt_mutate.data.frame <- function(.data, ..., by = NULL) {
  .data <- as_tidytable(.data)
  by <- enexpr(by)

  dt_mutate(.data, ..., by = !!by)
}
