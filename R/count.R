#' Count observations by group
#'
#' @description
#' Returns row counts of the dataset.
#'
#' `tally()` returns counts by group on a grouped tidytable.
#'
#' `count()` returns counts by group on a grouped tidytable, or column names can be specified
#' to return counts by group.
#'
#' @param .df A data.frame or data.table
#' @param ... Columns to group by in `count()`. `tidyselect` compatible.
#' @param wt Frequency weights.  `tidyselect` compatible.
#'   Can be `NULL` or a variable:
#'
#'   * If `NULL` (the default), counts the number of rows in each group.
#'   * If a variable, computes `sum(wt)` for each group.
#' @param sort If `TRUE`, will show the largest groups at the top.
#' @param name The name of the new column in the output.
#'
#'   If omitted, it will default to `n`.
#'
#' @export
#'
#' @examples
#' df <- data.table(
#'   x = c("a", "a", "b"),
#'   y = c("a", "a", "b"),
#'   z = 1:3
#' )
#'
#' df %>%
#'   count()
#'
#' df %>%
#'   count(x)
#'
#' df %>%
#'   count(where(is.character))
#'
#' df %>%
#'   count(x, wt = z, name = "x_sum")
#'
#' df %>%
#'   count(x, sort = TRUE)
#'
#' df %>%
#'   tally()
#'
#' df %>%
#'   group_by(x) %>%
#'   tally()
count <- function(.df, ..., wt = NULL, sort = FALSE, name = NULL) {
  count.(.df, ..., wt = {{ wt }}, sort = sort, name = name)
}

#' @export
#' @keywords internal
#' @inherit count
count. <- function(.df, ..., wt = NULL, sort = FALSE, name = NULL) {
  UseMethod("count.")
}

#' @export
count..tidytable <- function(.df, ..., wt = NULL, sort = FALSE, name = NULL) {
  .by <- quo(c(...))
  count_tally(.df, {{ wt }}, sort, name, .by = !!.by, .groups = "drop")
}

#' @export
count..grouped_tt <- function(.df, ..., wt = NULL, sort = FALSE, name = NULL) {
  count_tally(.df, {{ wt }}, sort, name, .by = NULL, .groups = "keep")
}

#' @export
count..data.frame <- function(.df, ..., wt = NULL, sort = FALSE, name = NULL) {
  .df <- as_tidytable(.df)
  count(.df, ..., wt = {{ wt }}, sort = sort, name = name)
}


#' @export
#' @rdname count
tally <- function(.df, wt = NULL, sort = FALSE, name = NULL) {
  tally.(.df, wt = {{ wt }}, sort = sort, name = name)
}

#' @export
#' @keywords internal
#' @inherit count
tally. <- function(.df, wt = NULL, sort = FALSE, name = NULL) {
  UseMethod("tally.")
}

#' @export
tally..tidytable <- function(.df, wt = NULL, sort = FALSE, name = NULL) {
  count_tally(.df, {{ wt }}, sort, name)
}

#' @export
tally..data.frame <- function(.df, wt = NULL, sort = FALSE, name = NULL) {
  .df <- as_tidytable(.df)
  tally(.df, wt = {{ wt }}, sort = sort, name = name)
}

count_tally <- function(.df, wt = NULL, sort = FALSE, name = NULL,
                        .by = NULL, .groups = "drop_last") {
  wt <- enquo(wt)

  if (is.null(name)) {
    name <- "n"
  }

  if (quo_is_null(wt)) {
    out <- summarize(.df, !!name := .N, .by = {{ .by }}, .groups = .groups)
  } else {
    out <- summarize(.df, !!name := sum(!!wt, na.rm = TRUE), .by = {{ .by }}, .groups = .groups)
  }

  if (sort) {
    out <- arrange(out, -!!sym(name))
  }

  out
}
