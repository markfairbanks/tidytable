#' Count observations by group
#'
#' @description
#' Returns row counts of the dataset. If bare column names are provided, `count.()` returns counts by group.
#'
#' @param .df A data.frame or data.table
#' @param ... Columns to group by. `tidyselect` compatible.
#'
#' @export
#' @md
#'
#' @examples
#' test_df <- data.table(
#'   x = 1:3,
#'   y = 4:6,
#'   z = c("a", "a", "b"))
#'
#' test_df %>%
#'   count.()
#'
#' test_df %>%
#'   count.(z)
#'
#' test_df %>%
#'   count.(where(is.character))
count. <- function(.df, ...) {
  UseMethod("count.")
}

#' @export
count..data.frame <- function(.df, ...) {

  .df <- as_tidytable(.df)

  .by <- enquos(...)

  summarize.(.df, N = .N, .by = c(!!!.by))

}
