#' Choose rows by position
#'
#' Choose rows by their ordinal position in a data.table. Grouped data.tables use the ordinal position within the group.
#'
#' @param dt_ A data.frame or data.table
#' @param rows Integer row values. Provide either positive values to keep, or negative values to drop. The values provided must be either all positive or all negative.
#' @param by A single unquoted column, or a `list()` of columns to group by.
#'
#' @return A data.table
#' @export
#'
#' @examples
#' library(data.table)
#'
#' example_dt <- data.table(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))
#'
#' # Select rows
#' dt_slice(example_dt, 1:10)
#'
#' # Select rows by group
#' dt_slice(example_dt, 1, by = z)
dt_slice <- function(dt_, rows = NULL, by = NULL) {

  is.data.frame(dt_) || stop("data must be a data.frame or data.table")

  if (missing(rows)) stop("rows must be supplied")
  if (!is.numeric(rows)) stop("rows must be a numeric vector")
  if(!is.data.table(dt_)) dt_ <- as.data.table(dt_)

  dt_ <- eval(dt_, parent.frame())
  if (missing(by)) {
    dt_[rows]
  } else {
    by <- characterize(substitute(by))
    dt_[, .SD[rows], by = by]
  }
}
