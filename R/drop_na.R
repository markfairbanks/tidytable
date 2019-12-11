#' Drop rows containing missing values
#'
#' @description
#' Drop rows containing missing values
#'
#' @param dt_ A data.frame or data.table
#' @param ... A selection of columns. If empty, all variables are selected. You can supply bare variable names.
#'
#' @return A data.table
#' @export
#'
#' @examples
#' library(data.table)
#' df <- data.table(x = c(1, 2, NA), y = c("a", NA, "b"))
#' df %>% dt_drop_na()
#' df %>% dt_drop_na(x)
dt_drop_na <- function(dt_, ...) {

  is.data.frame(dt_) || stop("data must be a data.frame or data.table")

  if (!is.data.table(dt_)) dt_ <- as.data.table(dt_)

  dots <- enlist_dots(...)

  if (length(dots) == 0) {
    na.omit(dt_)
  } else {
    for (var in dots) {
      var <- substitute(var)
      dt_ <- dt_[!is.na(eval(var))]
    }
    dt_
  }
}
