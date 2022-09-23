#' data.table::fcase() with vectorized default
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
#' df <- tidytable(x = 1:10)
#'
#' df %>%
#'   mutate(case_x = case(x < 5, 1,
#'                          x < 7, 2,
#'                          default = 3))
case <- function(..., default = NA) {
  dots <- list2(...)
  dots_length <- length(dots)

  if (dots_length %% 2 != 0) {
    abort("The length of conditions does not equal the length of values")
  }

  conditions_locs <- as.logical(seq_len(dots_length) %% 2)

  conditions <- dots[conditions_locs]
  size <- vec_size_common(!!!conditions)
  conditions <- vec_recycle_common(!!!conditions, .size = size)

  values <- dots[!conditions_locs]
  ptype <- vec_ptype_common(!!!values)
  values <- vec_cast_common(!!!values, .to = ptype)

  pairs <- vec_interleave(conditions, values)

  if (length(default) == 1) {
    default <- vec_cast(default, ptype)
    out <- exec(fcase, !!!pairs, default = default)
  } else {
    .default_condition <- vec_recycle(TRUE, size)
    .default_value <- vec_cast(default, ptype)

    out <- exec(fcase, !!!pairs, .default_condition, .default_value)
  }

  out
}

#' @export
#' @keywords internal
#' @inherit case
case. <- case
