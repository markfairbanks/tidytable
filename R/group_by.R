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
  .df <- .df_as_tidytable(.df)

  if (is_ungrouped(.df)) {
    tt_group_by(.df, ..., .add = .add)
  } else {
    if (is_true(.add)) {
      .groups <- group_vars(.df)
      out <- ungroup(.df)
      tt_group_by(out, all_of(.groups), ...)
    } else {
      out <- ungroup(.df)
      tt_group_by(out, ...)
    }
  }
}

#' @export
#' @keywords internal
#' @inherit group_by
group_by. <- function(.df, ..., .add = FALSE) {
  deprecate_dot_fun()
  group_by(.df, ..., .add = .add)
}

tt_group_by <- function(.df, ..., .add = FALSE) {
  dots <- enquos(...)
  check_across(dots, "group_by")
  .groups <- tidyselect_names(.df, !!!dots)
  if (length(.groups) == 0) {
    out <- .df
  } else {
    out <- set_class(.df, c("grouped_tt", tidytable_class()))
    out <- set_attr(out, "groups", .groups)
  }
  out
}

#' @export
#' @rdname group_by
ungroup <- function(.df, ...) {
  .df <- .df_as_tidytable(.df)

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
#' @keywords internal
#' @inherit group_by
ungroup. <- function(.df, ...) {
  deprecate_dot_fun()
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

#' @export
#' @keywords internal
#' @inherit group_vars
group_vars. <- function(x) {
  deprecate_dot_fun()
  group_vars(x)
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

#' @export
#' @keywords internal
#' @inherit is_grouped_df
is_grouped_df. <- function(x) {
  deprecate_dot_fun()
  is_grouped_df(x)
}
