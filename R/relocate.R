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
#' df <- data.table(
#'   a = 1:3,
#'   b = 1:3,
#'   c = c("a", "a", "b"),
#'   d = c("a", "a", "b")
#' )
#'
#' df %>%
#'   relocate.(c, .before = b)
#'
#' df %>%
#'   relocate.(a, b, .after = c)
#'
#' df %>%
#'   relocate.(where(is.numeric), .after = c)
relocate. <- function(.df, ..., .before = NULL, .after = NULL) {
  UseMethod("relocate.")
}

#' @export
relocate..tidytable <- function(.df, ..., .before = NULL, .after = NULL) {
  .before <- enquo(.before)
  .after <- enquo(.after)

  uses_before <- !quo_is_null(.before)
  uses_after <- !quo_is_null(.after)

  if (uses_before && uses_after) {
    abort("Must supply only one of `.before` and `.after`")
  }

  if (!uses_before && !uses_after) {
    .before <- quo(1)
    uses_before <- TRUE
  }

  all_locs <- seq_along(.df)
  relocate_locs <- tidyselect_locs(.df, ...)

  if (length(relocate_locs) == 0) {
    return(.df)
  }

  if (uses_before) {
    before <- min(tidyselect_locs(.df, !!.before))

    start_locs <- all_locs[all_locs < before]
  } else {
    after <- max(tidyselect_locs(.df, !!.after))

    start_locs <- all_locs[all_locs <= after]
  }

  start_locs <- setdiff(start_locs, relocate_locs)
  final_order <- vec_unique(c(start_locs, relocate_locs, all_locs))

  df_col_order(.df, final_order)
}

#' @export
relocate..data.frame <- function(.df, ..., .before = NULL, .after = NULL) {
  .df <- as_tidytable(.df)
  relocate.(.df, ..., .before = {{ .before }}, .after = {{ .after }})
}

