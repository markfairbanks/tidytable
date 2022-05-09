#' Case when
#'
#' @description
#' This function allows you to use multiple if/else statements in one call.
#'
#' It is called like `dplyr::case_when()`, but utilizes `data.table::fifelse()`
#' in the background for improved performance.
#'
#' @param ... A sequence of two-sided formulas. The left hand side gives the conditions,
#' the right hand side gives the values.
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
case_when. <- function(...) {
  dots <- list2(...)
  dots_length <- length(dots)
  if (dots_length == 0) abort("No cases provided.")

  # Extract default value
  default_bool <- map_lgl.(dots, ~ is_true(f_lhs(.x)))

  if (all(default_bool == FALSE)) {
    default_val <- NA
  } else {
    default_val <- dots[default_bool][[1]]
    default_val <- eval_tidy(f_rhs(default_val), env = caller_env())
  }

  # Remove default value from queries
  dots <- dots[!default_bool]

  conditions <- map.(dots, f_lhs)
  values <- map.(dots, f_rhs)

  pairs <- vec_interleave(conditions, values)
  pairs <- map.(pairs, eval_tidy, env = caller_env())

  case.(!!!pairs, default = default_val)
}
