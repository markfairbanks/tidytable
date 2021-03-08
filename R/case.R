#' Improved data.table::fcase()
#'
#' @description
#' This function allows you to use multiple if/else statements in one call.
#'
#' It is called like `data.table::fcase()`, but allows the user to use
#' a vector as the `default` argument.
#'
#' @param ... Sequence of condition/value designations
#' @param default Default value. Set to NA by default.
#'
#' @export
#'
#' @examples
#' test_df <- tidytable(x = 1:10)
#'
#' test_df %>%
#'   mutate.(case_x = case.(x < 5, 1,
#'                          x < 7, 2,
#'                          default = 3))
case. <- function(..., default = NA) {
  dots <- enquos(...)
  dots_length <- length(dots)

  odd_index <- as.logical(seq_len(dots_length) %% 2)
  even_index <- !odd_index

  conditions <- dots[odd_index]
  values <- dots[even_index]

  if (length(conditions) == 0) abort("No conditions supplied")
  if (length(values) == 0) abort("No values supplied")

  if (length(conditions) != length(values)) {
    abort("The length of conditions does not equal the length of values")
  }

  calls <- default

  for (i in rev(seq_along(conditions))) {
    calls <- call2(
      "ifelse.",
      call2('%|%', conditions[[i]], FALSE),
      values[[i]],
      calls
    )
  }

  eval_tidy(calls)
}
