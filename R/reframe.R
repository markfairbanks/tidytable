#' Reframe a data frame
#'
#' @description
#' Reframe a data frame. Note this is a simple alias for `summarize()`
#' that always returns an ungrouped tidytable.
#'
#' @param .df A data.frame or data.table
#' @param ... Aggregations to perform
#' @param .by Columns to group by
#'
#' @export
#' @examples
#' mtcars %>%
#'   reframe(qs = quantile(disp, c(0.25, 0.75)),
#'           prob = c(0.25, 0.75),
#'           .by = cyl)
reframe <- function(.df, ..., .by = NULL) {
  out <- summarize(.df, ..., .by = {{ .by }}, .sort = FALSE)
  ungroup(out)
}
