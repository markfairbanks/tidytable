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
#' test_df <- data.table(a = 1:3,
#'                       b = 4:6,
#'                       z = c("a", "a", "b"))
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
summarize_across. <- function(.df, .cols = everything(), .fns, ...,
                              .by = NULL, .names = NULL) {
  UseMethod("summarize_across.")
}

#' @export
summarize_across..data.frame <- function(.df, .cols = everything(), .fns, ...,
                                         .by = NULL, .names = NULL) {

  .df <- as_tidytable(.df)

  .cols <- select_vec_chr(.df, {{ .cols }})

  .by <- select_vec_chr(.df, {{ .by }})

  .cols <- .cols[.cols %notin% .by]

  if (length(.cols) == 0) abort("No columns have been selected to summarize.()")

  if (!is.list(.fns)) {

    .names <- .names %||% "{.col}"

    .col_names <- vec_as_names(
      glue(.names, .col = .cols, .fn = "1", col = .cols, fn = "1"),
      repair = "check_unique", quiet = TRUE
    )

    .fns <- as_function(.fns)

    .df <- eval_quo(
      .df[, lapply(.SD, .fns, ...), .SDcols = .cols, by = .by]
    )

    names(.df) <- c(.by, .col_names)

  } else {

    # Make new names
    names_flag <- have_name(.fns)

    if (!all(names_flag)) names(.fns)[!names_flag] <- seq_len(length(.fns))[!names_flag]

    fn_names <- names(.fns)

    .names <- .names %||% "{.col}_{.fn}"

    new_names <- unlist(map.(fn_names, ~ glue(.names, .col = .cols, .fn = .x, col = .cols, fn = .x)))

    # Convert .fns to list of map./lapply calls
    .fns <- map.(.fns, ~ call2('map.', quo(.SD), .x))

    .df <- eval_quo(
      .df[, c(!!!.fns), .SDcols = .cols, by = .by]
    )

    names(.df) <- c(.by, new_names)
  }
  .df
}

#' @export
#' @rdname summarize_across.
summarise_across. <- summarize_across.
