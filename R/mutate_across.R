#' Mutate multiple columns simultaneously
#'
#' @description
#' Mutate multiple columns simultaneously.
#'
#' @param .df A data.frame or data.table
#' @param .cols vector `c()` of unquoted column names. `tidyselect` compatible.
#' @param .fns Functions to pass. Can pass a list of functions.
#' @param ... Other arguments for the passed function
#' @param .by Columns to group by
#' @param .names A glue specification that helps with renaming output columns.
#' `{.col}` stands for the selected column, and `{.fn}` stands for the name of the function being applied.
#' The default (`NULL`) is equivalent to `"{.col}"` for a single function case and `"{.col}_{.fn}"`
#' when a list is used for `.fns`.
#'
#' @export
#'
#' @examples
#' test_df <- data.table(
#'   x = c(1,1,1),
#'   y = c(2,2,2),
#'   z = c("a", "a", "b"))
#'
#' test_df %>%
#'   mutate_across.(where(is.numeric), as.character)
#'
#' test_df %>%
#'   mutate_across.(c(x, y), ~ .x * 2)
#'
#' test_df %>%
#'   mutate_across.(everything(), as.character)
#'
#' test_df %>%
#'   mutate_across.(c(x, y), list(new = ~ .x * 2,
#'                                another = ~ .x + 7))
#' test_df %>%
#'   mutate_across.(
#'     .cols = c(x, y),
#'     .fns = list(new = ~ .x * 2, another = ~ .x + 7),
#'     .names = "{.col}_test_{.fn}"
#'   )
mutate_across. <- function(.df, .cols = everything(), .fns, ...,
                           .by = NULL, .names = NULL) {
  UseMethod("mutate_across.")
}

#' @export
mutate_across..data.frame <- function(.df, .cols = everything(), .fns, ...,
                                      .by = NULL, .names = NULL) {

  .df <- as_tidytable(.df)

  .cols <- select_vec_chr(.df, {{ .cols }})

  data_env <- env(quo_get_env(enquo(.fns)), .df = .df)

  dots <- enquos(...)

  if (length(.cols) == 0) return(.df)

  .fun <- enexpr(.fns)

  call_list <- across_calls(.fns, .fun, .cols, .names, dots)

  result_expr <- reset_expr(
    tidytable::mutate.(.df, !!!call_list, .by = {{ .by }})
  )

  eval_tidy(result_expr, new_data_mask(data_env), caller_env())
}
