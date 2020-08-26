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
#' @param by This argument has been renamed to .by and is deprecated
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
#'   mutate_across.(c(x, y),
#'                  .fns = list(new = ~ .x * 2, another = ~ .x + 7),
#'                  .names = "{.col}_test_{.fn}")
mutate_across. <- function(.df, .cols = everything(), .fns, ...,
                           .by = NULL, .names = NULL, by = NULL) {
  UseMethod("mutate_across.")
}

#' @export
mutate_across..data.frame <- function(.df, .cols = everything(), .fns, ...,
                                      .by = NULL, .names = NULL, by = NULL) {

  .df <- as_tidytable(.df)
  .df <- shallow(.df)

  .cols <- select_vec_chr(.df, {{ .cols }})

  .by <- check_dot_by(enquo(.by), enquo(by), "mutate_across.")
  .by <- select_vec_chr(.df, !!.by)

  .cols <- .cols[.cols %notin% .by]

  if (length(.cols) == 0) return(.df)

  if (!is.list(.fns)) {

    .names <- .names %||% "{.col}"

    .col_names <- vec_as_names(
      glue(.names, .col = .cols, .fn = "1", col = .cols, fn = "1"),
      repair = "check_unique", quiet = TRUE
    )

    eval_quo(
      .df[, (.col_names) := map.(.SD, .fns, ...), .SDcols = !!.cols, by = !!.by],
    )

  } else {

    names_flag <- have_name(.fns)

    if (!all(names_flag)) names(.fns)[!names_flag] <- seq_len(length(.fns))[!names_flag]

    fn_names <- names(.fns)

    .names <- .names %||% "{.col}_{.fn}"

    for (i in seq_along(fn_names)) {

      .col_names <- vec_as_names(
        glue(.names, .col = .cols, .fn = fn_names[[i]], col = .cols, fn = fn_names[[i]]),
        repair = "check_unique", quiet = TRUE
      )

      if (any(.col_names %in% names(.df))) {
        overwrite_cols <- .col_names[.col_names %in% names(.df)]

        warn(glue("Newly created column {overwrite_cols} overwrote an existing column called {overwrite_cols}"))
        warn("\nThis occurred due to the auto-naming feature of mutate_across.()")
        warn("\nTo avoid this use a named list in the .fns arg or adjust .names arg\n")
      }

      eval_quo(
        .df[, (.col_names) := map.(.SD, .fns[[i]]), .SDcols = !!.cols, by = !!.by],
      )
    }
  }
  .df[]
}

#' @export
#' @rdname dt_verb
#' @inheritParams mutate_across.
dt_mutate_across <- function(.df, .cols = everything(), .fns, ..., .by = NULL, by = NULL) {
  deprecate_warn("0.5.2", "tidytable::dt_mutate_across()", "mutate_across.()")

  .by <- check_dot_by(enquo(.by), enquo(by))
  mutate_across.(.df, .cols = {{ .cols }}, .fns, ..., .by = {{ .by }})
}
