#' Expand a data.table to use all combinations of values
#'
#' @description
#' Generates all combinations of variables found in a dataset.
#'
#' `expand()` is useful in conjunction with joins:
#' * use with `right_join()` to convert implicit missing values to explicit missing values
#' * use with `anti_join()` to find out which combinations are missing
#'
#' `nesting()` is a helper that only finds combinations already present in the dataset.
#'
#' @param .df A data.frame or data.table
#' @param ... Columns to get combinations of
#' @param .name_repair Treatment of duplicate names. See `?vctrs::vec_as_names` for options/details
#' @param .by Columns to group by
#'
#' @export
#'
#' @examples
#' df <- tidytable(x = c(1, 1, 2), y = c(1, 1, 2))
#'
#' df %>%
#'   expand(x, y)
#'
#' df %>%
#'   expand(nesting(x, y))
expand <- function(.df, ..., .name_repair = "check_unique", .by = NULL) {
  .df <- .df_as_tidytable(.df)

  if (is_ungrouped(.df)) {
    tt_expand(.df, ..., .name_repair = .name_repair, .by = {{ .by }})
  } else {
    .by <- group_vars(.df)
    out <- tt_expand(.df, ..., .name_repair = .name_repair, .by = any_of(.by))
    group_by(out, any_of(.by))
  }
}

#' @export
#' @keywords internal
#' @inherit expand
expand. <- function(.df, ..., .name_repair = "check_unique", .by = NULL) {
  deprecate_dot_fun()
  expand(.df, ..., .name_repair = .name_repair, .by = {{ .by }})
}

tt_expand <- function(.df, ..., .name_repair = "check_unique", .by = NULL) {
  dots <- enquos(...)
  dots <- dots[!map_lgl(dots, quo_is_null)]
  if (length(dots) == 0) return(.df)

  .by <- enquo(.by)

  if (quo_is_null(.by)) {
    df_expand(.df, !!!dots, .name_repair = .name_repair)
  } else {
    .by <- tidyselect_names(.df, !!.by)

    out <- .df[, df_expand(.SD, !!!dots, .name_repair = "minimal"), by = .by]

    df_name_repair(out, .name_repair)
  }
}

df_expand <- function(.df, ..., .name_repair = .name_repair) {
  dots <- enquos(...)

  expr <- call2("crossing", !!!dots, .name_repair = .name_repair, .ns = "tidytable")

  eval_tidy(expr, .df)
}

#' @export
#' @rdname expand
nesting <- function(..., .name_repair = "check_unique") {
  cols <- dots_list(..., .named = TRUE)
  out <- df_name_repair(new_tidytable(cols), .name_repair)
  out <- distinct(out)
  setorder(out, na.last = TRUE)
  out
}

#' @export
#' @keywords internal
#' @inherit expand
nesting. <- nesting
