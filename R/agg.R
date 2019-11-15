#' Aggregation
#'
#' Simple alias for aggregation functions in data.table
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' dt <- data.table(x = c(1,2,3), y = c("a","a","b"))
#' dt[, agg(avg_x = mean(x)), by = y]
agg <- function(...) {
  list(...)
}

#' @export
vars <- base::list
