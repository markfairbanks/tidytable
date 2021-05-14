#' Expand a data.table to use all combinations of values
#'
#' @description
#' Generates all combinations of variables found in a dataset.
#'
#' `expand.()` is useful in conjunction with joins:
#' * use with `right_join.()` to convert implicit missing values to explicit missing values
#' * use with `anti_join.()` to find out which combinations are missing
#'
#' `nesting.()` is a helper that only finds combinations already present in the dataset.
#'
#' @param .df A data.frame or data.table
#' @param ... Columns to get combinations of
#' @param .name_repair Treatment of duplicate names. See `?vctrs::vec_as_names` for options/details
#'
#' @export
#'
#' @examples
#' test_df <- tidytable(x = c(1, 1, 2), y = c(1, 1, 2))
#'
#' test_df %>%
#'   expand.(x, y)
#'
#' test_df %>%
#'   expand.(nesting.(x, y))
expand. <- function(.df, ..., .name_repair = "check_unique") {
  UseMethod("expand.")
}

#' @export
expand..tidytable <- function(.df, ..., .name_repair = "check_unique") {
  dots <- enquos(...)
  dots <- dots[!map_lgl.(dots, quo_is_null)]
  if (length(dots) == 0) return(.df)

  dt_env <- build_dt_env(dots, !!!.df)

  dots <- map.(dots, quo_squash)

  out <- call2("crossing.", !!!dots, .name_repair = .name_repair, .ns = "tidytable")

  eval_tidy(out, env = dt_env)
}

#' @export
expand..data.frame <- function(.df, ..., .name_repair = "check_unique") {
  .df <- as_tidytable(.df)
  expand.(.df, ..., .name_repair = .name_repair)
}

#' @export
#' @rdname expand.
nesting. <- function(..., .name_repair = "check_unique") {
  cols <- dots_list(..., .named = TRUE)

  out <- tidytable(!!!cols, .name_repair = .name_repair)
  out <- distinct.(out)
  setorder(out)
  out
}
