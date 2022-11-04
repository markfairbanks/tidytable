#' Selection helper for grouping columns
#'
#' @description
#' Selection helper for grouping columns
#'
#' @export
#'
#' @examples
#' df <- tidytable(
#'   x = c("a", "b", "c"),
#'   y = 1:3,
#'   z = 1:3
#' )
#'
#' df %>%
#'   group_by(x) %>%
#'   select(group_cols(), y)
group_cols <- function() {
  all_of(group_vars(peek_data()))
}
