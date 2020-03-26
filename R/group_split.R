#' Split data frame by groups
#'
#' @description
#' Split data frame by groups. Returns a list
#'
#' Supports enhanced selection
#'
#' @param .data A data.frame or data.table
#' @param ... Groups to split by
#' @export
#'
#' @examples
#' test_df <- data.table::data.table(
#'   a = 1:5,
#'   b = 1:5,
#'   c = c("a","a","a","b","b"),
#'   d = c("a","a","a","b","b"))
#'
#' test_df %>%
#'   group_split.(c, d)
group_split. <- function(.data, ...) {
  UseMethod("group_split.")
}

#' @export
group_split..tidytable <- function(.data, ...) {

  dots <- enexprs(...)

  if (length(dots) == 0) {
    list(.data)
  } else {
    dots <- as.character(dots_selector(.data, ...))

    unname(split(.data, by = dots))
  }
}

#' @export
group_split..data.frame <- function(.data, ...) {
  .data <- as_tidytable(.data)

  group_split.(.data, ...)
}

#' @export
#' @rdname group_split.
dt_group_split <- group_split.


