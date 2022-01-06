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
  .df <- shallow(.df)

  vec_assert(prefix, logical(), 1)
  vec_assert(prefix_sep, character(), 1)
  vec_assert(drop_first, logical(), 1)
  vec_assert(dummify_na, logical(), 1)

  cols <- tidyselect_syms(.df, {{ cols }})

  original_cols <- copy(names(.df))

  ordered_cols <- character()

  for (col in cols) {
    col_name <- as.character(col)

    if (drop_first) {
      unique_vals <- vec_unique(as.character(.df[[col_name]]))[-1]
    } else {
      unique_vals <- vec_unique(as.character(.df[[col_name]]))
    }

    # If NAs need dummies, convert to character string "NA" for col name creation
    if (dummify_na) {
      unique_vals <- unique_vals %|% "NA"
    } else {
      unique_vals <- unique_vals[!is.na(unique_vals)]
    }

    if (prefix) {
      new_names <- paste(col_name, unique_vals, sep = prefix_sep)
    } else {
      new_names <- unique_vals
    }

    # Remove "NA" from unique vals after new_names columns are made
    not_na_cols <- new_names[unique_vals != "NA"]
    unique_vals <- unique_vals[unique_vals != "NA"]

    dummy_calls <- vector("list", length(unique_vals))
    names(dummy_calls) <- not_na_cols
    for (i in seq_along(unique_vals)) {
      dummy_calls[[i]] <- expr(ifelse.(!!col == !!unique_vals[i], 1L, 0L, 0L))
    }

    .df <- mutate.(.df, !!!dummy_calls)

    # Since the prior step doesn't recognize NA as a character,
    # an extra step is needed to flag NA vals
    if (dummify_na) {
      na_col <- new_names[new_names %notin% not_na_cols]
      if (length(na_col) > 0) {
        .df <- mutate.(.df, !!na_col := as.integer(is.na(!!col)))
      }
    }

    new_names <- f_sort(new_names)

    ordered_cols <- c(ordered_cols, new_names)
  }

  final_order <- c(original_cols, ordered_cols)

  setcolorder(.df, final_order)

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
