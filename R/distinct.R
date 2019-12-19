#' Select distinct/unique rows
#'
#' @description
#' Simple alias to unique(). Retain only unique/distinct rows from an input df.
#'
#' @param .data A data.frame or data.table
#'
#' @import data.table
#' @return A data.table
#' @export
#'
#' @examples
#' dt %>% dt_distinct()
dt_distinct <- function(.data) {

  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  unique(.data)
}
