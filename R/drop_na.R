#' Drop rows containing missing values
#'
#' @description
#' Drop rows containing missing values
#'
#' Supports enhanced selection
#'
#' @param .data A data.frame or data.table
#' @param ... Optional: A selection of columns. If empty, all variables are selected.
#'
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
  UseMethod("dt_drop_na")
}

#' @export
dt_drop_na.tidytable <- function(.data, ...) {

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

#' @export
dt_drop_na.data.frame <- function(.data, ...) {
  .data <- as_tidytable(.data)

  dt_drop_na(.data, ...)
}
