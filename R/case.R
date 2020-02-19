#' Case when
#'
#' @description
#' This function allows you to use multiple if/else statements in one call.
#' Note that this function is called differently than `dplyr::case_when`! See examples
#'
#' @param ... Sequence of condition/value designations
#' @param default Default value. Set to NA. Argument must be named.
#'
#' @export
#'
#' @examples
#' library(data.table)
#'
#' test_df <- data.table::data.table(
#'   a = 1:10,
#'   b = 11:20,
#'   c = c(rep("a", 6), rep("b", 4)),
#'   d = c(rep("a", 4), rep("b", 6)))
#'
#' test_df %>%
#'   dt_mutate(x = dt_case(b < 13, 3,
#'                         a > 4, 2,
#'                         default = 10))
#' test_df %>%
#'   dt_mutate(x = dt_case(c == "a","a",
#'                         default = d))
dt_case <- function(..., default = NA) {
  dots <- enexprs(...)

  index <- '+'(1, 1:length(dots)) %% 2

  conditions <- dots[index == 0]
  values <- dots[index == 1]

  if (length(conditions) == 0) abort("No conditions supplied")
  if (length(values) == 0) abort("No values supplied")

  if (length(conditions) != length(values))
    abort("The length of conditions does not equal the length values")

  if (length(default) == 1) {
    if (is.na(default)) {
      na_class <- class(values[[1]])
      if (na_class == "logical") {
        default <- NA
      } else if (na_class == "complex") {
        default <- NA_complex_
      } else if (na_class == "integer") {
        default <- NA_integer_
      } else if (na_class == "character") {
        default <- NA_character_
      } else {
        default <- NA_real_
      }
    }
  }

  vals <- default

  for (i in seq_along(conditions)) {
    vals <- fifelse(eval(conditions[[i]], parent.frame()), eval(values[[i]], parent.frame()), vals)
  }

  vals
}
