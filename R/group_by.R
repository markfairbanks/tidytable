#' Grouping
#'
#' @description
#' * `group_by()` adds a grouping structure to a tidytable. Can use tidyselect syntax.
#' * `ungroup()` removes grouping.
#'
#' @param .df A data.frame or data.table
#' @param ... Columns to group by
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
group_by <- function(.df, ...) {
  group_by.(.df, ...)
}

#' @export
#' @keywords internal
#' @inherit group_by
group_by. <- function(.df, ...) {
  UseMethod("group_by.")
}

#' @export
group_by..tidytable <- function(.df, ...) {
  dots <- enquos(...)
  check_across(dots, "group_by")
  .groups <- tidyselect_names(.df, !!!dots)
  if (length(.groups) == 0) {
    return(.df)
  }
  out <- set_class(.df, c("grouped_tt", class(.df)))
  attr(out, "groups") <- .groups
  out
}

#' @export
group_by..grouped_tt <- function(.df, ...) {
  out <- ungroup(.df)
  group_by(out, ...)
}

#' @export
group_by..data.frame <- function(.df, ...) {
  .df <- as_tidytable(.df)
  group_by(.df, ...)
}

#' @export
#' @rdname group_by
ungroup <- function(.df) {
  ungroup.(.df)
}

#' @export
#' @keywords internal
#' @inherit group_by
ungroup. <- function(.df) {
  UseMethod("ungroup.")
}

#' @export
ungroup..data.frame <- function(.df) {
  attr(.df, "groups") <- NULL
  as_tidytable(.df)
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
group_vars. <- group_vars

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
is_grouped_df. <- is_grouped_df
