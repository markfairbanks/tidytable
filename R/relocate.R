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
#'   a = 1:5,
#'   b = 1:5,
#'   c = c("a","a","a","b","b"),
#'   d = c("a","a","a","b","b"))
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

  .before <- enquo(.before)
  .after <- enquo(.after)
  .df <- shallow(.df)

  if  (!quo_is_null(.before) && !quo_is_null(.after))
    stop("Must supply only one of `.before` and `.after`")

  if (quo_is_null(.before) && quo_is_null(.after))
    .before <- quo(1)

  data_names <- names(.df)
  all_cols_i <- seq_along(data_names)
  selected_cols_i <- select_dots_i(.df, ...)

  if (!quo_is_null(.before)) {

    before_i <- select_vec_i(.df, !!.before)
    start_cols_i <- all_cols_i[all_cols_i < before_i]

  } else {

    after_i <- select_vec_i(.df, !!.after)
    start_cols_i <- all_cols_i[all_cols_i <= after_i]

  }

  start_cols_i <- start_cols_i[start_cols_i %notin% selected_cols_i]
  final_order_i <- vec_unique(c(start_cols_i, selected_cols_i, all_cols_i))

  final_order <- data_names[final_order_i]

  setcolorder(.df, final_order)[]
}

#' @export
#' @rdname relocate.
dt_relocate <- function(.df, ..., .before = NULL, .after = NULL) {
  deprecate_soft("0.5.2", "tidytable::dt_relocate()", "relocate.()")

  relocate.(.df, ..., .before = {{ .before }}, .after = {{ .after }})
}
