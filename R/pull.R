#' Pull out a single variable
#'
#' @param dt_ A data.frame or data.table
#' @param column The column to pull from the data.table
#'
#' @return A vector
#' @import data.table
#' @export
#'
#' @examples
#' example_dt <- data.table(x = c(1,2,3), y = c(4,5,6))
#'
#' example_dt %>%
#'   dt_pull(y)
dt_pull <- function(dt_, column) {

  is.data.frame(data) || stop("data must be a data.frame or data.table")

  if (!is.data.table(data)) data <- as.data.table(data)

  dt_[, eval(substitute(column))]
}
