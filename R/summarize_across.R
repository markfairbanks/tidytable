#' Summarize multiple columns
#'
#' @description
#' Summarize multiple columns simultaneously
#'
#' @param .df A data.frame or data.table
#' @param .cols vector `c()` of unquoted column names. `tidyselect` compatible.
#' @param .fns Functions to pass. Can pass a list of functions.
#' @param ... Other arguments for the passed function
#' @param by Columns to group by
#'
#' @md
#' @export
#'
#' @examples
#' test_df <- tidytable(a = 1:3, b = 4:6, z = c("a", "a", "b"))
#'
#' # Can pass a function and arguments separately
#' test_df %>%
#'   summarize_across.(c(a, b), mean, na.rm = TRUE)
#'
#' # Or can use a purrr style formula
#' test_df %>%
#'   summarize_across.(c(a, b), ~ mean(.x, na.rm = TRUE))
#'
#' # Multiple functions can be passed in a list
#' test_df %>%
#'   summarize_across.(c(a, b), list(avg = mean, length), by = z)
#'
#' test_df %>%
#'   summarize_across.(where(is.numeric), mean, na.rm = TRUE)
summarize_across. <- function(.df, .cols = everything(), .fns, ..., by = NULL) {
  UseMethod("summarize_across.")
}

#' @export
summarize_across..data.frame <- function(.df, .cols = everything(), .fns, ..., by = NULL) {

  .df <- as_tidytable(.df)

  .cols <- select_vec_chr(.df, {{ .cols }})

  by <- select_vec_chr(.df, {{ by }})

  if (length(.cols) == 0) abort("No columns have been selected to summarize.()")

  # Needed to ensure summarize_across.() doesn't fail due to
  # integer results in one group but double results in another group
  .df <- mutate_across.(.df, where(is.numeric), as.double)

  if (!is.list(.fns)) {

    .df <- eval_quo(
      .df[, map.(.SD, .fns, ...), .SDcols = .cols, by = by]
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
      .df[, eval_quo({.SD = .env$.SD; c(!!!.fns)}, .SD), .SDcols = .cols, by = by]
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
