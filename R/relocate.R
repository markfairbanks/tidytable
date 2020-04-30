#' Relocate a column to a new position
#'
#' @description
#' Move a column or columns to a new position
#'
#' @param .data A data.frame or data.table
#' @param ... A selection of columns to move. `tidyselect` compatible.
#' @param .before Column to move selection before
#' @param .after Column to move selection after
#'
#' @export
#'
#' @examples
#' test_df <- data.table::data.table(
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
#'   relocate.(is.numeric, .after = c)
relocate. <- function(.data, ..., .before = NULL, .after = NULL) {
  UseMethod("relocate.")
}

#' @export
relocate..data.frame <- function(.data, ..., .before = NULL, .after = NULL) {

  .data <- as_tidytable(.data)

  .before <- enexpr(.before)
  .after <- enexpr(.after)
  .data <- shallow(.data)

  if  (!is.null(.before) && !is.null(.after))
    stop("Must supply only one of `.before` and `.after`")

  if (is.null(.before) && is.null(.after))
    .before <- 1

  data_names <- names(.data)
  all_cols_i <- seq_along(data_names)
  selected_cols_i <- dots_selector_i(.data, ...)

  if (!is.null(.before)) {

    before_i <- vec_selector_i(.data, !!.before)
    start_cols_i <- all_cols_i[all_cols_i < before_i]

  } else {

    after_i <- vec_selector_i(.data, !!.after)
    start_cols_i <- all_cols_i[all_cols_i <= after_i]

  }

  start_cols_i <- start_cols_i[start_cols_i %notin% selected_cols_i]
  final_order_i <- unique(c(start_cols_i, selected_cols_i, all_cols_i))

  final_order <- data_names[final_order_i]

  setcolorder(.data, final_order)[]
}

#' @export
#' @rdname relocate.
dt_relocate <- relocate.
