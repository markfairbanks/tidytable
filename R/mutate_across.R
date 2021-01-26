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

  # Need to capture separately for use later, otherwise a bare function call pre-evaluates
  # Ex: data.table mutate_across.(test_df, everything(), between, 1, 3)
  # R will search for data.table's Cbetween instead of the R function
  .fun <- enexpr(.fns)

  if (!is.list(.fns)) {

    if (is_anon_fun(.fns)) .fun <- as_function(.fns)

    call_list <- map.(syms(.cols), ~ call2(.fun, .x, !!!dots))

    .names <- .names %||% "{.col}"

    names(call_list) <- vec_as_names(
      glue(.names, .col = .cols, .fn = "1", col = .cols, fn = "1"),
      repair = "check_unique", quiet = TRUE
    )
  } else {
    names_flag <- have_name(.fns)

    if (!all(names_flag)) names(.fns)[!names_flag] <- seq_len(length(.fns))[!names_flag]

    .fns <- map.(.fns, as_function)

    fn_names <- names(.fns)

    combos <- expand_grid.(.fns = .fns, .cols = .cols)

    .fns <- combos$.fns

    .cols <- combos$.cols

    call_list <- map2.(.fns, syms(.cols), ~ call2(.x, .y, !!!dots))

    fn_names <- vec_rep_each(fn_names, length(.fns)/length(fn_names))

    .names <- .names %||% "{.col}_{.fn}"

    names(call_list) <- vec_as_names(
      glue(.names, .col = .cols, .fn = fn_names, col = .cols, fn = fn_names),
      repair = "check_unique", quiet = TRUE
    )
  }

  result_expr <- reset_expr(
    mutate.(.df, !!!call_list, .by = {{ .by }})
  )

  eval_tidy(result_expr, new_data_mask(data_env), caller_env())
}

# Converts result_expr to text and back to expression to avoid environment issues (#145)
reset_expr <- function(express) {
  parse_expr(quo_text(quo_squash(enquo(
    express
  ))))
}

is_anon_fun <- function(fun) {
  is.null(packageName(get_env(fun))) || is_bare_formula(fun)
}
