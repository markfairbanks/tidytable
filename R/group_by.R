#' Grouping
#'
#' @description
#' * `group_by()` adds a grouping structure to a tidytable. Can use tidyselect syntax.
#' * `ungroup()` removes grouping.
#'
#' @param .df A data.frame or data.table
#' @param ... Columns to group by
#' @param .add Should grouping cols specified be added to the current grouping
#'
#' @export
#'
#' @examples
#' df <- data.table(
#'   a = 1:3,
#'   b = 4:6,
#'   c = c("a", "a", "b"),
#'   d = c("a", "a", "b")
#' )
#'
#' df %>%
#'   group_by(c, d) %>%
#'   summarize(mean_a = mean(a)) %>%
#'   ungroup()
#'
#' # Can also use tidyselect
#' df %>%
#'   group_by(where(is.character)) %>%
#'   summarize(mean_a = mean(a)) %>%
#'   ungroup()
group_by <- function(.df, ..., .add = FALSE) {
  UseMethod("group_by")
}

#' @export
group_by.tidytable <- function(.df, ..., .add = FALSE) {
  dots <- enquos(...)
  check_no_across(dots)
  .groups <- tidyselect_names(.df, !!!dots, .allow_rename = FALSE)
  if (length(.groups) == 0) {
    out <- .df
  } else {
    out <- set_class(.df, c("grouped_tt", tidytable_class()))
    out <- set_attr(out, "groups", .groups)
  }
  out
}

#' @export
group_by.grouped_tt <- function(.df, ..., .add = FALSE) {
  if (is_true(.add)) {
    .groups <- group_vars(.df)
    out <- ungroup(.df)
    group_by(out, all_of(.groups), ...)
  } else {
    out <- ungroup(.df)
    group_by(out, ...)
  }
}

#' @export
group_by.data.frame <- function(.df, ..., .add = FALSE) {
  .df <- as_tidytable(.df)
  group_by(.df, ..., .add = .add)
}

#' @export
#' @rdname group_by
ungroup <- function(.df, ...) {
  UseMethod("ungroup")
}

#' @export
ungroup.tidytable <- function(.df, ...) {
  dots <- enquos(...)
  if (length(dots) == 0) {
    out <- set_attr(.df, "groups", NULL)
    as_tidytable(out)
  } else {
    cols_drop <- tidyselect_names(.df, !!!dots)
    groups <- setdiff(group_vars(.df), cols_drop)
    group_by(.df, all_of(groups))
  }
}

#' @export
ungroup.data.frame <- function(.df, ...) {
  .df <- as_tidytable(.df)
  ungroup(.df, ...)
}

#' Get the grouping variables
#'
#' @description
#' Get the grouping variables
#'
#' @param x A grouped tidytable
#'
#' @export
#'
#' @examples
#' df <- data.table(
#'   a = 1:3,
#'   b = 4:6,
#'   c = c("a", "a", "b"),
#'   d = c("a", "a", "b")
#' )
#'
#' df %>%
#'   group_by(c, d) %>%
#'   group_vars()
group_vars <- function(x) {
  attr(x, "groups")
}

#' Check if the tidytable is grouped
#'
#' @description
#' Check if the tidytable is grouped
#'
#' @param x An object
#'
#' @export
#'
#' @examples
#' df <- data.table(
#'   a = 1:3,
#'   b = c("a", "a", "b")
#' )
#'
#' df %>%
#'   group_by(b) %>%
#'   is_grouped_df()
is_grouped_df <- function(x) {
  inherits(x, "grouped_tt")
}
