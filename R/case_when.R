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
#' test_df <- tidytable(x = 1:10)
#'
#' test_df %>%
#'   mutate.(case_x = case_when.(x < 5 ~ 1,
#'                               x < 7 ~ 2,
#'                               TRUE ~ 3))
case_when. <- function(...) {
  dots <- list2(...)
  dots_length <- length(dots)
  if (dots_length == 0) abort("No cases provided.")

  # Extract default value
  default_flag <- map_lgl.(dots, ~ isTRUE(f_lhs(.x)))

  if (all(default_flag == FALSE)) default_val <- TRUE ~ NA
  else default_val <- dots[default_flag][[1]]

  default_env <- caller_env()
  default_val <- new_quosure(f_rhs(default_val), env = f_env(default_val) %||% default_env)

  # Remove default value from queries
  dots <- dots[!default_flag]

  dots_length <- length(dots)

  conditions <- vector("list", dots_length)
  values <- vector("list", dots_length)

  quos_pairs <- map.(dots, validate_formula, default_env)

  for (i in seq_len(dots_length)) {
    pair <- quos_pairs[[i]]

    conditions[[i]] <- pair$lhs
    values[[i]] <- pair$rhs
  }

  case_call <- vector("list", dots_length * 2)

  odd_index <- as.logical(seq_along(case_call) %% 2)
  even_index <- !odd_index

  case_call[odd_index] <- conditions
  case_call[even_index] <- values

  case.(!!!case_call, default = default_val)
}

validate_formula <- function(x, default_env) {

  if (!is_formula(x)) {
    abort("input must be a formula")
  }
  if (is_null(f_lhs(x))) {
    abort("formulas must be two-sided")
  }

  list(
    lhs = new_quosure(f_lhs(x), default_env),
    rhs = new_quosure(f_rhs(x), default_env)
  )
}
