#' Count observations by group
#'
#' @description
#' Returns row counts of the dataset. If bare column names are provided, `count.()` returns counts by group.
#'
#' @param .df A data.frame or data.table
#' @param ... Columns to group by. `tidyselect` compatible.
#' @param wt Frequency weights.  `tidyselect` compatible.
#'   Can be `NULL` or a variable:
#'
#'   * If `NULL` (the default), counts the number of rows in each group.
#'   * If a variable, computes `sum(wt)` for each group.
#' @param sort If `TRUE`, will show the largest groups at the top.
#' @param name The name of the new column in the output.
#'
#'   If omitted, it will default to `N`.
#' @export
#' @md
#'
#' @examples
#' df <- data.table(
#'   x = 1:3,
#'   y = 4:6,
#'   z = c("a", "a", "b")
#' )
#'
#' df %>%
#'   count.()
#'
#' df %>%
#'   count.(z)
#'
#' df %>%
#'   count.(where(is.character))
#'
#' df %>%
#'   count.(z, wt = y, name = "y_sum")
#'
#' df %>%
#'   count.(z, sort = TRUE)
count. <- function(.df, ..., wt = NULL, sort = FALSE, name = NULL) {
  UseMethod("count.")
}

#' @export
count..tidytable <- function(.df, ..., wt = NULL, sort = FALSE, name = NULL) {
  .by <- enquos(...)
  wt <- enquo(wt)

  if (is.null(name)) {
    name <- "N"
  }

  if (quo_is_null(wt)) {
    .df <- summarize.(.df, !!name := .N, .by = c(!!!.by))
  } else {
    .df <- summarize.(.df, !!name := sum(!!wt, na.rm = TRUE), .by = c(!!!.by))
  }

  if (sort) {
    .df <- arrange.(.df, -!!sym(name))
  }

  .df
}

#' @export
count..data.frame <- function(.df, ..., wt = NULL, sort = FALSE, name = NULL) {
  .df <- as_tidytable(.df)
  count.(.df, ..., wt = {{ wt }}, sort = sort, name = name)
}
