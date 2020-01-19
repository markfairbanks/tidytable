#' Select distinct/unique rows
#'
#' @description
#' Simple alias to unique(). Retain only unique/distinct rows from an input df.
#'
#' @param .data A data.frame or data.table
#' @param ... Columns to select before determining uniqueness. If omitted, will use all columns
#'
#' @import data.table
#' @return A data.table
#' @export
#'
#' @examples
#' dt %>% dt_distinct()
dt_distinct <- function(.data, ...) {

  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  dots <- enexprs(...)

  if (length(dots) == 0) {
    unique(.data)
  } else {
    .data %>%
      dt_select(!!!dots) %>%
      unique()
  }
}
