#' Select top (or bottom) n rows (by value)
#'
#' @description
#' Select the top or bottom entries in each group, ordered by `wt`.
#'
#' @param .df A data.frame or data.table
#' @param n Number of rows to return
#' @param wt Optional. The variable to use for ordering. If NULL uses the last column in the data.table.
#' @param .by Columns to group by
#'
#' @export
#'
#' @examples
#' df <- data.table(
#'   x = 1:5,
#'   y = 6:10,
#'   z = c(rep("a", 3), rep("b", 2))
#' )
#'
#' df %>%
#'   top_n(2, wt = y)
#'
#' df %>%
#'   top_n(2, wt = y, .by = z)
top_n <- function(.df, n = 5, wt = NULL, .by = NULL) {
  UseMethod("top_n")
}

#' @export
top_n.tidytable <- function(.df, n = 5, wt = NULL, .by = NULL) {
  wt <- enquo(wt)

  if (quo_is_null(wt)) {
    slice_head(.df, {{ n }}, {{ .by }})
  } else {
    slice_max(.df, order_by = !!wt, {{ n }}, .by = {{ .by }})
  }
}

#' @export
top_n.data.frame <- function(.df, n = 5, wt = NULL, .by = NULL) {
  .df <- as_tidytable(.df)
  top_n(.df, {{ n }}, {{ wt }}, .by = {{ .by }})
}

#' @export top_n.
#' @keywords internal
#' @usage
#' top_n(.df, n = 5, wt = NULL, .by = NULL)
#' @inherit top_n title description params examples
top_n. <- function(.df, n = 5, wt = NULL, .by = NULL) {
  UseMethod("top_n.")
}

#' @exportS3Method top_n. data.frame
top_n..data.frame <- function(.df, n = 5, wt = NULL, .by = NULL) {
  top_n(.df, {{ n }}, {{ wt }}, .by = {{ .by }})
}
