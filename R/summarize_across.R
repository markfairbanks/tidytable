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
#' @param by This argument has been renamed to .by and is deprecated
#'
#' @md
#' @export
#'
#' @examples
#' test_df <- data.table(a = 1:3,
#'                       b = 4:6,
#'                       z = c("a", "a", "b"))
#'
#' # Single function
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
#'                          max_plus_one = ~ max(.x) + 1),
#'                     .by = z)
summarize_across. <- function(.df, .cols = everything(), .fns, ..., .by = NULL, by = NULL) {
  UseMethod("summarize_across.")
}

#' @export
summarize_across..data.frame <- function(.df, .cols = everything(), .fns, ..., .by = NULL, by = NULL) {

  .df <- as_tidytable(.df)

  .cols <- select_vec_chr(.df, {{ .cols }})

  .by <- check_dot_by(enquo(.by), enquo(by), "summarize_across.")
  .by <- select_vec_chr(.df, !!.by)

  if (length(.cols) == 0) abort("No columns have been selected to summarize.()")

  # Needed to ensure summarize_across.() doesn't fail due to
  # integer results in one group but double results in another group
  # .df <- mutate_across.(.df, where(is.numeric), as.double)

  if (!is.list(.fns)) {

    .fns <- as_function(.fns)

    .df <- eval_quo(
      .df[, lapply(.SD, .fns, ...), .SDcols = .cols, by = .by]
    )

  } else {

    names_flag <- have_name(.fns)

    if (!all(names_flag)) {

      # If some functions are unnamed, create names
      # The resulting names are prefixed with "fn" instead of suffixed
      # Prefixing is the data.table default when providing names,
      # which is different than dplyr but might not be worth fixing
      names(.fns)[!names_flag] <- "fn"

      fixed_names <- vec_as_names(names(.fns), repair = "unique", quiet = TRUE)
      fixed_names <- str_replace(fixed_names, "\\...", "")

      names(.fns) <- fixed_names
    }

    # Convert .fns to list of map./lapply calls
    .fns <- map.(.fns, ~ call2('map.', quo(.SD), .x))

    .df <- eval_quo(
      .df[, c(!!!.fns), .SDcols = .cols, by = .by]
    )

    old_names <- names(.df)
    new_names <- str_replace(old_names, "[.]", "_")

    setnames(.df, old_names, new_names)
  }
  .df
}

#' @export
#' @rdname summarize_across.
summarise_across. <- summarize_across.
