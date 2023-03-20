#' `data.table::fcase()` with vectorized default
#'
#' @description
#' This function allows you to use multiple if/else statements in one call.
#'
#' It is called like `data.table::fcase()`, but allows the user to use
#' a vector as the `default` argument.
#'
#' @param ... Sequence of condition/value designations
#' @param default Default value. Set to NA by default.
#' @param ptype Optional ptype to specify the output type.
#' @param size Optional size to specify the output size.
#'
#' @export
#'
#' @examples
#' df <- tidytable(x = 1:10)
#'
#' df %>%
#'   mutate(case_x = case(x < 5, 1,
#'                        x < 7, 2,
#'                        default = 3))
case <- function(..., default = NA, ptype = NULL, size = NULL) {
  dots <- list2(...)
  dots_length <- length(dots)

  if (dots_length %% 2 != 0) {
    abort("The length of conditions does not equal the length of values")
  }

  is_condition <- as.logical(seq_len(dots_length) %% 2)

  conditions <- dots[is_condition]
  size <- vec_size_common(!!!conditions, .size = size)
  conditions <- vec_recycle_common(!!!conditions, .size = size)

  values <- dots[!is_condition]
  ptype <- vec_ptype_common(!!!values, default, .ptype = ptype)

  values <- vec_cast_common(!!!values, .to = ptype)

  pairs <- vec_interleave(conditions, values)

  .default <- vec_cast(default, ptype)

  if (length(default) == 1) {
    out <- exec(fcase, !!!pairs, default = .default)
  } else {
    .default_condition <- vec_recycle(TRUE, size)
    out <- exec(fcase, !!!pairs, .default_condition, .default)
  }

  out
}

#' @export
#' @keywords internal
#' @inherit case
case. <- function(..., default = NA, ptype = NULL, size = NULL) {
  deprecate_dot_fun()
  case(..., default = default, ptype = ptype, size = size)
}
