#' Split data frame by groups
#'
#' @description
#' Split data frame by groups. Returns a list.
#'
#' @param .data A data.frame or data.table
#' @param ... Columns to group and split by. `tidyselect` compatible.
#' @param keep Should the grouping columns be kept
#'
#' @export
#' @md
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
#'
#' test_df %>%
#'   group_split.(c, d, keep = FALSE)
group_split. <- function(.data, ..., keep = TRUE) {
  UseMethod("group_split.")
}

#' @export
group_split..data.frame <- function(.data, ..., keep = TRUE) {

  .data <- as_tidytable(.data)

  dots <- enexprs(...)

  if (length(dots) == 0) {
    list(.data)
  } else {
    dots <- as.character(dots_selector(.data, ...))

    dots <- unname(split(.data, by = dots, keep.by = keep))

    map.(dots, as_tidytable)
  }
}

#' @export
#' @rdname group_split.
dt_group_split <- group_split.


