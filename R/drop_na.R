#' Drop rows containing missing values
#'
#' @description
#' Drop rows containing missing values
#'
#' @param .data A data.frame or data.table
#' @param ... A selection of columns. If empty, all variables are selected. You can supply bare variable names.
#'
#' @import data.table
#' @return A data.table
#' @export
#'
#' @examples
#' library(data.table)
#' df <- data.table(x = c(1, 2, NA), y = c("a", NA, "b"))
#' df %>% dt_drop_na()
#' df %>% dt_drop_na(x)
dt_drop_na <- function(.data, ...) {

  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")

  if (!is.data.table(.data)) .data = as.data.table(.data)

  dots <- enlist_dots(...)

  if (length(dots) == 0) {
    na.omit(.data)
  } else {
    for (var in dots) {
      var <- substitute(var)
      .data <- .data[!is.na(eval(var))]
    }
    .data
  }
}
