#' Select distinct/unique rows
#'
#' @description
#' Retain only unique/distinct rows from an input df.
#'
#' @param .df A data.frame or data.table
#' @param ... Columns to select before determining uniqueness. If omitted, will use all columns.
#' `tidyselect` compatible.
#' @param .keep_all Only relevant if columns are provided to ... arg.
#' This keeps all columns, but only keeps the first row of each distinct
#' values of columns provided to ... arg.
#'
#' @export
#' @md
#'
#' @examples
#' test_df <- tidytable(
#'   x = 1:3,
#'   y = 4:6,
#'   z = c("a", "a", "b"))
#'
#' test_df %>%
#'   distinct.()
#'
#' test_df %>%
#'   distinct.(z)
distinct. <- function(.df, ..., .keep_all = FALSE) {
  UseMethod("distinct.")
}

#' @export
distinct..data.frame <- function(.df, ..., .keep_all = FALSE) {

  .df <- as_tidytable(.df)

  vec_assert(.keep_all, logical(), 1)

  dots <- enquos(...)

  if (length(dots) == 0) {

    .df <- unique(.df)

  } else {

    select_cols <- select_dots_idx(.df, ...)

    .df <- unique(.df, by = select_cols)

    if (!.keep_all) .df <- .df[, ..select_cols]

    named_flag <- have_name(dots)

    if (any(named_flag)) {

      named_dots <- dots[named_flag]

      .df <- rename.(.df, !!!named_dots)
    }
  }

  .df
}

globalVariables("..select_cols")
