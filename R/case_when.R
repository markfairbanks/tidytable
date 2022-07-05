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
#'   mutate.(case_x = case_when.(x < 5 ~ 1,
#'                               x < 7 ~ 2,
#'                               TRUE ~ 3))
case_when. <- function(..., .default = NA, .ptype = NULL, .size = NULL) {
  dots <- list2(...)
  dots_length <- length(dots)
  if (dots_length == 0) abort("No cases provided.")

  is_default <- map_lgl.(dots, ~ is_true(f_lhs(.x)))
  if (any(is_default) && dots_length > 1) {
    .default <- dots[is_default][[1]]
    .default <- eval_tidy(f_rhs(.default), env = caller_env())

    dots <- dots[!is_default]
  }

  conditions <- map.(dots, f_lhs)
  conditions <- map.(conditions, eval_tidy, env = caller_env())

  values <- map.(dots, f_rhs)
  values <- map.(values, eval_tidy, env = caller_env())

  if (!is.null(.ptype)) {
    values <- vec_cast_common(!!!values, .to = .ptype)
  }

  pairs <- vec_interleave(conditions, values)

  out <- case.(!!!pairs, default = .default)

  if (!is.null(.size)) {
    out <- vec_recycle(out, .size)
  }

  out
}
