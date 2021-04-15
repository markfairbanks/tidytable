#' Relocate a column to a new position
#'
#' @description
#' Move a column or columns to a new position
#'
#' @param .df A data.frame or data.table
#' @param ... A selection of columns to move. `tidyselect` compatible.
#' @param .before Column to move selection before
#' @param .after Column to move selection after
#'
#' @export
#'
#' @examples
#' test_df <- data.table(
#'   a = 1:3,
#'   b = 1:3,
#'   c = c("a", "a", "b"),
#'   d = c("a", "a", "b")
#' )
#'
#' test_df %>%
#'   relocate.(c, .before = b)
#'
#' test_df %>%
#'   relocate.(a, b, .after = c)
#'
#' test_df %>%
#'   relocate.(where(is.numeric), .after = c)
relocate. <- function(.df, ..., .before = NULL, .after = NULL) {
  UseMethod("relocate.")
}

#' @export
relocate..data.frame <- function(.df, ..., .before = NULL, .after = NULL) {
  .df <- as_tidytable(.df)
  .df <- shallow(.df)

  .before <- enquo(.before)
  .after <- enquo(.after)

  before_is_null <- quo_is_null(.before)
  after_is_null <- quo_is_null(.after)

  if  (!before_is_null && !after_is_null) {
    stop("Must supply only one of `.before` and `.after`")
  }

  if (before_is_null && after_is_null) {
    .before <- quo(1)
  }

  data_names <- names(.df)
  all_cols_i <- seq_along(data_names)
  selected_cols_i <- select_dots_idx(.df, ...)

  if (!before_is_null) {
    before_i <- select_vec_idx(.df, !!.before)

    start_cols_i <- all_cols_i[all_cols_i < before_i]
  } else {
    after_i <- select_vec_idx(.df, !!.after)

    start_cols_i <- all_cols_i[all_cols_i <= after_i]
  }

  start_cols_i <- start_cols_i[start_cols_i %notin% selected_cols_i]
  final_order_i <- vec_unique(c(start_cols_i, selected_cols_i, all_cols_i))

  final_order <- data_names[final_order_i]

  setcolorder(.df, final_order)

  .df
}

