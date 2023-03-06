#' Case when
#'
#' @description
#' This function allows you to use multiple if/else statements in one call.
#'
#' It is called like `dplyr::case_when()`, but utilizes `data.table::fifelse()`
#' in the background for improved performance.
#'
#' @param ... A sequence of two-sided formulas. The left hand side gives the conditions,
#'   the right hand side gives the values.
#' @param .default The default value if all conditions evaluate to `FALSE`.
#' @param .ptype Optional ptype to specify the output type.
#' @param .size Optional size to specify the output size.
#'
#' @export
#'
#' @examples
#' df <- tidytable(x = 1:10)
#'
#' df %>%
#'   mutate(case_x = case_when(x < 5 ~ 1,
#'                             x < 7 ~ 2,
#'                             TRUE ~ 3))
case_when <- function(..., .default = NA, .ptype = NULL, .size = NULL) {
  dots <- list2(...)
  dots_length <- length(dots)
  if (dots_length == 0) abort("No cases provided.")

  is_default <- map_lgl(dots, ~ is_true(f_lhs(.x)))
  if (any(is_default) && dots_length > 1) {
    .default <- dots[is_default][[1]]
    .default <- f_rhs(.default)

    dots <- dots[!is_default]
  }

  conditions <- map(dots, f_lhs)

  values <- map(dots, f_rhs)

  pairs <- vec_interleave(conditions, values)

  out <- call2("case", !!!pairs, default = .default, ptype = .ptype, size = .size, .ns = "tidytable")
  out <- as_quosure(out, get_dt_env(enquos(...)))

  eval_tidy(out)
}

#' @export
#' @keywords internal
#' @inherit case_when
case_when. <- function(..., .default = NA, .ptype = NULL, .size = NULL) {
  deprecate_dot_fun()
  case_when(..., .default = .default, .ptype = .ptype, .size = .size)
}
