#' Count
#'
#' Count the numbers of observations within groups
#'
#' @param dt_ the data table to uncount
#' @param ... groups
#' @param na.rm should any rows with missingness be removed before the count? Default is `FALSE`.
#' @param wt the wt assigned to the counts (same number of rows as the data)
#'
#' @examples
#'
#' library(data.table)
#' dt <- data.table(
#'   x = rnorm(1e5),
#'   y = runif(1e5),
#'   grp = sample(1L:3L, 1e5, replace = TRUE),
#'   wt = runif(1e5, 1, 100)
#'   )
#'
#' dt_count(dt, grp)
#' dt_count(dt, grp, na.rm = TRUE)
#' dt_count(dt, grp, na.rm = TRUE, wt = wt)
#'
#' @import data.table
#' @importFrom stats complete.cases
#'
#' @export
dt_count <- function(dt_, ..., na.rm = FALSE, wt = NULL){

 dt_ <- as_dt(dt_)

  dots <- substitute(list(...))
  wt <- substitute(wt)

  if (na.rm)
    dt_ <- dt_[complete.cases(dt_)]

  if (!is.null(wt))
    return(dt_[, list(N = sum(eval(wt))), keyby = eval(dots)])

  dt_[, .N, keyby = eval(dots)]
}
