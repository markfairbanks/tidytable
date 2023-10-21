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
#' df <- tidytable(
#'   a = 1:3,
#'   b = 1:3,
#'   c = c("a", "a", "b"),
#'   d = c("a", "a", "b")
#' )
#'
#' df %>%
#'   group_split(c, d)
#'
#' df %>%
#'   group_split(c, d, .keep = FALSE)
#'
#' df %>%
#'   group_split(c, d, .named = TRUE)
group_split <- function(.df, ..., .keep = TRUE, .named = FALSE) {
  UseMethod("group_split")
}

#' @export
group_split.tidytable <- function(.df, ..., .keep = TRUE, .named = FALSE) {
  by <- select(.df, ...)

  if (is_false(.keep)) {
    .df <- select(.df, -all_of(names(by)))
  }

  split <- vec_split(.df, by)

  out <- split$val

  if (is_true(.named)) {
    names <- exec(paste, !!!split$key, sep = "_")
    names(out) <- names
  }

  out
}

#' @export
group_split.grouped_tt <- function(.df, ..., .keep = TRUE, .named = FALSE) {
  .by <- group_vars(.df)
  out <- ungroup(.df)
  group_split(out, all_of(.by), .keep = .keep, .named = .named)
}

#' @export
group_split.data.frame <- function(.df, ..., .keep = TRUE, .named = FALSE) {
  .df <- as_tidytable(.df)
  group_split(.df, ..., .keep = .keep, .named = .named)
}


