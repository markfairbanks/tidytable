#' Select distinct/unique rows
#'
#' @description
#' Simple alias to unique(). Retain only unique/distinct rows from an input df.
#'
#' @param dt_ A data.frame or data.table
#'
#' @import data.table
#' @return A data.table
#' @export
#'
#' @examples
#' dt %>% dt_distinct()
dt_distinct <- function(dt_) {

  is.data.frame(dt_) || stop("data must be a data.frame or data.table")

  if (!is.data.table(dt_)) dt_ <- as.data.table(dt_)

  unique(dt_)
}
