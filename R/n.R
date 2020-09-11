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

  eval_tidy(expr(.N), env = caller_env())
}

#' @export
#' @rdname dt_verb
#' @inheritParams n.
dt_n <- function() {
  deprecate_stop("0.5.2", "tidytable::dt_n()", "n.()")

  n.()
}

# deprecate_soft("0.5.3", "tidytable::n.()",
#                details = paste(
#                  c("Using n.() is much slower than the data.table helper .N and should be avoided.",
#                    "",
#                    "  # Good",
#                    "  df %>% summarize.(count = .N)",
#                    "",
#                    "  # Bad",
#                    "  df %>% summarize.(count = n.())"), collapse = "\n"))
