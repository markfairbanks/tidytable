#' Vectorized `switch()`
#'
#' @description
#' Allows the user to more succinctly create conditions bu `case_when()`.
#'
#' @param .x A vector
#' @param ... A sequence of two-sided formulas. The left hand side gives the old values,
#'   the right hand side gives the new value.
#' @param .default The default value if all conditions evaluate to `FALSE`.
#' @param .ptype Optional ptype to specify the output type.
#'
#' @export
#'
#' @examples
#' df <- tidytable(x = c("a", "b", "c", "d"))
#'
#' df %>%
#'   mutate(case_x = case_match(x,
#'                              c("a", "b") ~ "new_1",
#'                              "c" ~ "new_2",
#'                              .default = x))
case_match <- function(.x, ..., .default = NA, .ptype = NULL) {
  .x <- enquo(.x)
  dots <- list2(...)
  dots <- map(dots, prep_case_match_dot, .x)
  case_when(!!!dots, .default = .default, .ptype = .ptype)
}

#' @export
#' @keywords internal
#' @inherit case_match
case_match. <- case_match

prep_case_match_dot <- function(dot, .x) {
  lhs <- f_lhs(dot)
  lhs <- call2("%in%", .x, lhs, .ns = "tidytable")
  f_lhs(dot) <- lhs
  dot
}
