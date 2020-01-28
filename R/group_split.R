#' Split data frame by groups
#'
#' @description
#' Split data frame by groups. Returns a list
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
#'   dt_group_split(c, d)
dt_group_split <- function(.data, ...) {
  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  dots <- enexprs(...)

  if (length(dots) == 0) {
    list(.data)
  } else {
    dots <- dots_selector(.data, ...) %>%
      as.character()

    unname(split(.data, by = dots))
  }
}
