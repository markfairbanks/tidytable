#' Convert character and factor columns to dummy variables
#'
#' @description
#' Convert character and factor columns to dummy variables
#'
#' @param .df A data.frame or data.table
#' @param cols A single column or a vector of unquoted columns to dummify.
#' Defaults to all character & factor columns using `c(where(is.character), where(is.factor))`.
#' `tidyselect` compatible.
#' @param prefix TRUE/FALSE - If TRUE, a prefix will be added to new column names
#' @param prefix_sep Separator for new column names
#' @param drop_first TRUE/FALSE - If TRUE, the first dummy column will be dropped
#' @param dummify_na TRUE/FALSE - If TRUE, NAs will also get dummy columns
#'
#' @export
#'
#' @examples
#' df <- tidytable(
#'   chr = c("a", "b", NA),
#'   fct = as.factor(c("a", NA, "c")),
#'   num = 1:3
#' )
#'
#' # Automatically does all character/factor columns
#' df %>%
#'   get_dummies()
#'
#' df %>%
#'   get_dummies(cols = chr)
#'
#' df %>%
#'   get_dummies(cols = c(chr, fct), drop_first = TRUE)
#'
#' df %>%
#'   get_dummies(prefix_sep = ".", dummify_na = FALSE)
get_dummies <- function(.df,
                        cols = where(~ is.character(.x) | is.factor(.x)),
                        prefix = TRUE,
                        prefix_sep = "_",
                        drop_first = FALSE,
                        dummify_na = TRUE) {
  .df <- .df_as_tidytable(.df)

  cols <- tidyselect_syms(.df, {{ cols }})

  for (col in cols) {
    col_name <- as.character(col)

    unique_vals <- vec_unique(.df[[col_name]])
    if (is.factor(unique_vals)) {
      unique_vals <- as.character(unique_vals)
    }

    if (drop_first) {
      unique_vals <- unique_vals[-1]
    }

    unique_vals <- f_sort(unique_vals)

    len <- length(unique_vals)

    # Due to above f_sort NA will be the last value if it exists
    any_na <- vec_detect_missing(unique_vals[len])

    if (any_na) {
      unique_vals <- unique_vals[-len]
    }

    if (prefix) {
      cols_complete <- paste(col_name, unique_vals, sep = prefix_sep)
      col_na <- paste(col_name, "NA", sep = prefix_sep)
    } else {
      cols_complete <- as.character(unique_vals)
      col_na <- "NA"
    }

    if (any_na) {
      is_complete <- vec_detect_complete(.df[[col_name]])
      .df <- dt_j(
        .df,
        (cols_complete) := lapply(unique_vals, function(.x) as.integer(.x == !!col & ..is_complete))
      )
    } else {
      .df <- dt_j(
        .df,
        (cols_complete) := lapply(unique_vals, function(.x) as.integer(.x == !!col))
      )
    }

    if (dummify_na && any_na) {
      .df <- dt_j(.df, (col_na) := as.integer(!..is_complete))
    }
  }

  .df
}

#' @export
#' @keywords internal
#' @inherit get_dummies
get_dummies. <- function(.df,
                         cols = where(~ is.character(.x) | is.factor(.x)),
                         prefix = TRUE,
                         prefix_sep = "_",
                         drop_first = FALSE,
                         dummify_na = TRUE) {
  deprecate_dot_fun()
  get_dummies(.df, {{ cols }}, prefix, prefix_sep, drop_first, dummify_na)
}

globalVariables("..is_complete")


