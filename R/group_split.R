#' Split data frame by groups
#'
#' @description
#' Split data frame by groups. Returns a list.
#'
#' @param .df A data.frame or data.table
#' @param ... Columns to group and split by. `tidyselect` compatible.
#' @param .keep Should the grouping columns be kept
#'
#' @export
#' @md
#'
#' @examples
#' test_df <- tidytable(
#'   a = 1:5,
#'   b = 1:5,
#'   c = c("a","a","a","b","b"),
#'   d = c("a","a","a","b","b"))
#'
#' test_df %>%
#'   group_split.(c, d)
#'
#' test_df %>%
#'   group_split.(c, d, .keep = FALSE)
group_split. <- function(.df, ..., .keep = TRUE) {
  UseMethod("group_split.")
}

#' @export
group_split..data.frame <- function(.df, ..., .keep = TRUE) {

  .df <- as_tidytable(.df)

  vec_assert(.keep, logical(), 1)

  dots <- enquos(...)

  if (length(dots) == 0) {
    list(.df)
  } else {
    dots <- select_dots_chr(.df, ...)

    dots <- unname(split(.df, by = dots, keep.by = .keep))

    map.(dots, as_tidytable)
  }
}

