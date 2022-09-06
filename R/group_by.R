#' Grouping
#'
#' @description
#' * `group_by.()` adds a grouping structure to a tidytable. Can use tidyselect syntax.
#' * `ungroup.()` removes grouping.
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
#'   group_by.(c, d) %>%
#'   summarize.(mean_a = mean(a)) %>%
#'   ungroup.()
#'
#' # Can also use tidyselect
#' df %>%
#'   group_by.(where(is.character)) %>%
#'   summarize.(mean_a = mean(a)) %>%
#'   ungroup.()
group_by. <- function(.df, ...) {
  UseMethod("group_by.")
}

#' @export
group_by..tidytable <- function(.df, ...) {
  .groups <- tidyselect_names(.df, ...)
  if (length(.groups) == 0) {
    return(.df)
  }
  class(.df) <- c("grouped_tt", class(.df))
  attr(.df, "groups") <- .groups
  .df
}

#' @export
group_by..grouped_tt <- function(.df, ...) {
  .groups <- tidyselect_names(.df, ...)
  if (length(.groups) == 0) {
    return(ungroup.(.df))
  }
  attr(.df, "groups") <- .groups
  .df
}

#' @export
group_by..data.frame <- function(.df, ...) {
  .df <- as_tidytable(.df)
  group_by.(.df, ...)
}

#' @export
#' @rdname group_by.
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
#' @param .df A tidytable
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
#'   group_by.(c, d) %>%
#'   group_vars.()
group_vars. <- function(.df) {
  attr(.df, "groups")
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
#'   group_by.(b) %>%
#'   is_grouped_df.()
is_grouped_df. <- function(x) {
  inherits(x, "grouped_tt")
}