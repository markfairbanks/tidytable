#' Uncount a data.table
#'
#' @param .df A data.frame or data.table
#' @param weights A column containing the weights to uncount by
#' @param .remove If TRUE removes the selected `weights` column
#' @param .id A string name for a new column containing a unique identifier for the newly uncounted rows.
#'
#' @export
#'
#' @examples
#' df <- data.table(x = c("a", "b"), n = c(1, 2))
#'
#' uncount.(df, n)
#'
#' uncount.(df, n, .id = "id")
uncount. <- function(.df, weights, .remove = TRUE, .id = NULL) {
  UseMethod("uncount.")
}

#' @export
uncount..tidytable <- function(.df, weights, .remove = TRUE, .id = NULL) {
  weights <- enquo(weights)

  .reps <- pull.(.df, !!weights)

  result_df <- vec_rep_each(.df, .reps)

  if (!is.null(.id)) {
    result_df <- dt_j(result_df, !!.id := sequence(.reps))
  }

  if (.remove) {
    result_df <- dt_j(result_df, !!weights := NULL)
  }

  result_df
}

#' @export
uncount..data.frame <- function(.df, weights, .remove = TRUE, .id = NULL) {
  .df <- as_tidytable(.df)
  uncount.(.df, {{ weights }}, .remove, .id)
}
