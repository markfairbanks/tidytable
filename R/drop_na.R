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
#'   drop_na.()
#'
#' df %>%
#'   drop_na.(x)
#'
#' df %>%
#'   drop_na.(is.numeric)
drop_na. <- function(.data, ...) {
  UseMethod("drop_na.")
}

#' @export
drop_na..tidytable <- function(.data, ...) {

  dots <- enexprs(...)

  if (length(dots) == 0) {
    na.omit(.data)
  } else {
    dots <- dots_selector(.data, ...)

    for (dot in dots) {
      .data <- eval_expr(
        .data[!is.na(!!dot)]
      )
    }
    .data
  }
}

#' @export
drop_na..data.frame <- function(.data, ...) {
  .data <- as_tidytable(.data)

  drop_na.(.data, ...)
}

#' @export
#' @rdname drop_na.
dt_drop_na <- drop_na.
