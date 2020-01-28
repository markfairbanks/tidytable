#' Drop rows containing missing values
#'
#' @description
#' Drop rows containing missing values
#'
#' Supports enhanced selection
#'
#' @param .data A data.frame or data.table
#' @param ... A selection of columns. If empty, all variables are selected.
#'
#' @return A data.table
#' @export
#'
#' @examples
#' df <- data.table::data.table(
#'   x = c(1,2,NA),
#'   y = c("a",NA,"b"))
#'
#' df %>%
#'   dt_drop_na()
#'
#' df %>%
#'   dt_drop_na(x)
#'
#' df %>%
#'   dt_drop_na(is.numeric)
dt_drop_na <- function(.data, ...) {

  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  dots <- enexprs(...)

  if (length(dots) == 0) {
    na.omit(.data)
  } else {
    dots <- dots_selector(.data, ...)

    for (dot in dots) {
      .data <- eval_tidy(expr(
        .data[!is.na(!!dot)]
      ))
    }
    .data
  }
}
