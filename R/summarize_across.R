#' Summarize multiple columns
#'
#' @description
#' Summarize multiple columns simultaneously
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
#'   a = 1:3,
#'   b = 4:6,
#'   z = c("a", "a", "b")
#' )
#'
#' # Pass a single function
#' test_df %>%
#'   summarize_across.(c(a, b), mean, na.rm = TRUE)
#'
#' # Single function using purrr style interface
#' test_df %>%
#'   summarize_across.(c(a, b), ~ mean(.x, na.rm = TRUE))
#'
#' # Passing a list of functions (with .by)
#' test_df %>%
#'   summarize_across.(c(a, b), list(mean, max), .by = z)
#'
#' # Passing a named list of functions (with .by)
#' test_df %>%
#'   summarize_across.(c(a, b),
#'                     list(avg = mean,
#'                          max = ~ max(.x)),
#'                     .by = z)
#'
#' # Use the `.names` argument for more naming control
#' test_df %>%
#'   summarize_across.(c(a, b),
#'                     list(avg = mean,
#'                          max = ~ max(.x)),
#'                     .by = z,
#'                     .names = "{.col}_test_{.fn}")
summarize_across. <- function(.df, .cols = everything(), .fns = NULL, ...,
                              .by = NULL, .names = NULL) {
  UseMethod("summarize_across.")
}

#' @export
summarize_across..data.frame <- function(.df, .cols = everything(), .fns = NULL, ...,
                                         .by = NULL, .names = NULL) {
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

  summarize.(.df, !!!call_list, .by = !!.by)
}

#' @export
#' @rdname summarize_across.
summarise_across. <- summarize_across.
