#' Split data frame by groups
#'
#' @description
#' Split data frame by groups. Returns a list.
#'
#' @param .df A data.frame or data.table
#' @param ... Columns to group and split by. `tidyselect` compatible.
#' @param .keep Should the grouping columns be kept
#' @param .named _experimental_: Should the list be named with labels that identify the group
#'
#' @export
#'
#' @examples
#' test_df <- tidytable(
#'   a = 1:3,
#'   b = 1:3,
#'   c = c("a","a","b"),
#'   d = c("a","a","b")
#' )
#'
#' test_df %>%
#'   group_split.(c, d)
#'
#' test_df %>%
#'   group_split.(c, d, .keep = FALSE)
#'
#' test_df %>%
#'   group_split.(c, d, .named = TRUE)
group_split. <- function(.df, ..., .keep = TRUE, .named = FALSE) {
  UseMethod("group_split.")
}

#' @export
group_split..tidytable <- function(.df, ..., .keep = TRUE, .named = FALSE) {
  vec_assert(.keep, logical(), 1)
  vec_assert(.named, logical(), 1)

  dots <- enquos(...)

  if (length(dots) == 0) {
    list(.df)
  } else {
    by <- tidyselect_names(.df, !!!dots)

    dots <- split(.df, by = by, keep.by = .keep)

    if (.named) {
      names(dots) <- str_replace_all.(names(dots), ".", "_", fixed = TRUE)
    } else {
      dots <- unname(dots)
    }

    map.(dots, tidytable_restore, .df)
  }
}

#' @export
group_split..data.frame <- function(.df, ..., .keep = TRUE, .named = FALSE) {
  .df <- as_tidytable(.df)
  group_split.(.df, ..., .keep = .keep, .named = .named)
}

