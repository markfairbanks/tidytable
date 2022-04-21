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
#' @md
#' @export
#'
#' @examples
#' df <- tidytable(
#'   col1 = c("a", "b", "c", NA),
#'   col2 = as.factor(c("a", "b", NA, "d")),
#'   var1 = rnorm(4, 0, 1)
#' )
#'
#' # Automatically does all character/factor columns
#' df %>%
#'   get_dummies.()
#'
#' # Can select one column
#' df %>%
#'   get_dummies.(col1)
#'
#' # Can select one or multiple columns in a vector of unquoted column names
#' df %>%
#'   get_dummies.(c(col1, col2))
#'
#' # Can drop certain columns using
#' df %>%
#'   get_dummies.(c(where(is.character), -col2))
#'
#' df %>%
#'   get_dummies.(prefix_sep = ".", drop_first = TRUE)
#'
#' df %>%
#'   get_dummies.(c(col1, col2), dummify_na = FALSE)
get_dummies. <- function(.df,
                         cols = c(where(is.character), where(is.factor)),
                         prefix = TRUE,
                         prefix_sep = "_",
                         drop_first = FALSE,
                         dummify_na = TRUE) {
  UseMethod("get_dummies.")
}

#' @export
get_dummies..tidytable <- function(.df,
                                   cols = c(where(is.character), where(is.factor)),
                                   prefix = TRUE,
                                   prefix_sep = "_",
                                   drop_first = FALSE,
                                   dummify_na = TRUE) {
  cols <- tidyselect_syms(.df, {{ cols }})

  for (col in cols) {
    col_name <- as.character(col)

    unique_vals <- vec_unique(as.character(.df[[col_name]]))

    if (drop_first) {
      unique_vals <- unique_vals[-1]
    }

    unique_vals <- f_sort(unique_vals)

    # If NAs need dummies, convert to character string "NA" for col name creation
    na_loc <- is.na(unique_vals)
    any_na <- any(na_loc)
    if (dummify_na && any_na) {
      unique_vals[na_loc] <- "NA"
    } else if (any_na) {
      unique_vals <- unique_vals[!na_loc]
    }

    if (prefix) {
      new_names <- paste(col_name, unique_vals, sep = prefix_sep)
    } else {
      new_names <- unique_vals
    }

    # Remove "NA" from unique vals after new_names columns are made
    if (dummify_na && any_na) {
      not_na_cols <- new_names[!na_loc]
      unique_vals <- unique_vals[!na_loc]
    } else {
      not_na_cols <- new_names
    }

    if (any_na) {
      .df <- dt(.df, ,
                (not_na_cols) :=
                  lapply(unique_vals, function(.x) as.integer(.x == !!col & !is.na(!!col)))
                )
    } else {
      .df <- dt(.df, ,
                (not_na_cols) :=
                  lapply(unique_vals, function(.x) as.integer(.x == !!col))
                )
    }

    # Since the prior step doesn't recognize NAs,
    # an extra step is needed to flag NA vals
    if (dummify_na && any_na) {
      na_col <- new_names[na_loc]
      .df <- dt(.df, , (na_col) := as.integer(is.na(!!col)))
    }
  }

  .df
}

#' @export
get_dummies..data.frame <- function(.df,
                                    cols = c(where(is.character), where(is.factor)),
                                    prefix = TRUE,
                                    prefix_sep = "_",
                                    drop_first = FALSE,
                                    dummify_na = TRUE) {
  .df <- as_tidytable(.df)
  get_dummies.(.df, {{ cols }}, prefix, prefix_sep, drop_first, dummify_na)
}

globalVariables("where")
