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
#' df <- data.table(
#'   x = rep(1, 3),
#'   y = rep(2, 3),
#'   z = c("a", "a", "b")
#' )
#'
#' df %>%
#'   mutate_across.(where(is.numeric), as.character)
#'
#' df %>%
#'   mutate_across.(c(x, y), ~ .x * 2)
#'
#' df %>%
#'   mutate_across.(everything(), as.character)
#'
#' df %>%
#'   mutate_across.(c(x, y), list(new = ~ .x * 2,
#'                                another = ~ .x + 7))
#' df %>%
#'   mutate_across.(
#'     .cols = c(x, y),
#'     .fns = list(new = ~ .x * 2, another = ~ .x + 7),
#'     .names = "{.col}_test_{.fn}"
#'   )
mutate_across. <- function(.df, .cols = everything(), .fns = NULL, ...,
                           .by = NULL, .names = NULL) {
  UseMethod("mutate_across.")
}

#' @export
mutate_across..data.frame <- function(.df, .cols = everything(), .fns = NULL, ...,
                                      .by = NULL, .names = NULL) {
  deprecate_old_across("mutate")

  .df <- as_tidytable(.df)

  .by <- enquo(.by)

  .cols <- get_across_cols(.df, enquo(.cols), {{ .by }})
  if (length(.cols) == 0) return(.df)

  dots <- enquos(...)

  dt_env <- get_dt_env(dots)

  .fns <- enexpr(.fns)
  if (is_null(.fns)) return(.df)

  call_list <- across_calls(.fns, .cols, .names, dots)

  call_list <- as_quosures(call_list, dt_env)

  mutate.(.df, !!!call_list, .by = {{ .by }})
}
