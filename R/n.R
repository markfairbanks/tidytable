#' Number of observations in each group
#'
#' @description
#' Helper function that can be used to find counts by group.
#'
#' Can be used inside `summarize.()`, `mutate.()`, & `filter.()`
#'
#' @export
#'
#' @examples
#' test_df <- data.table(
#'   x = c(1,2,3),
#'   y = c(4,5,6),
#'   z = c("a","a","b"))
#'
#' test_df %>%
#'   summarize.(count = n.(),
#'              .by = z)
#'
#' test_df %>%
#'   mutate.(count = n.())
n. <- function() {
  eval_expr(.N, env = caller_env())
}
