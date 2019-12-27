#' Summarize helpers
#'
#' @description
#' These are made to mimic dplyr's `summarize()` helpers
#'
#' @md
#' @return A data.table
#' @export
#'
#' @examples
#' example_dt %>%
#'   dt_summarize(count = dt_n())
dt_n <- function() {
  eval.parent(substitute(.N))
}
