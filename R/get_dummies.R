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
#' test_df <- tidytable(
#'   col1 = c("a", "b", "c", NA),
#'   col2 = as.factor(c("a", "b", NA, "d")),
#'   var1 = rnorm(4,0,1))
#'
#' # Automatically does all character/factor columns
#' test_df %>%
#'   get_dummies.()
#'
#' # Can select one column
#' test_df %>%
#'   get_dummies.(col1)
#'
#' # Can select one or multiple columns in a vector of unquoted column names
#' test_df %>%
#'   get_dummies.(c(col1, col2))
#'
#' # Can drop certain columns using
#' test_df %>%
#'   get_dummies.(c(where(is.character), -col2))
#'
#' test_df %>%
#'   get_dummies.(prefix_sep = ".", drop_first = TRUE)
#'
#' test_df %>%
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
get_dummies..data.frame <- function(.df,
                                    cols = c(where(is.character), where(is.factor)),
                                    prefix = TRUE,
                                    prefix_sep = "_",
                                    drop_first = FALSE,
                                    dummify_na = TRUE) {

  .df <- as_tidytable(.df)
  .df <- shallow(.df)

  vec_assert(prefix, logical(), 1)
  vec_assert(prefix_sep, character(), 1)
  vec_assert(drop_first, logical(), 1)
  vec_assert(dummify_na, logical(), 1)

  cols <- select_vec_sym(.df, {{ cols }})

  original_cols <- copy(names(.df))

  ordered_cols <- character(0)

  for (col in cols) {

    col_name <- as.character(col)

    if (drop_first) unique_vals <- vec_unique(as.character(.df[[col_name]]))[-1]
    else unique_vals <- vec_unique(as.character(.df[[col_name]]))

    # If NAs need dummies, convert to character string "NA" for col name creation
    if (dummify_na) unique_vals <- unique_vals %|% "NA"
    else unique_vals <- unique_vals[!is.na(unique_vals)]

    if (prefix) new_names <- str_c.(col_name, unique_vals, sep = prefix_sep)
    else new_names <- unique_vals

    # Remove "NA" from unique vals after new_names columns are made
    not_na_cols <- new_names[unique_vals != "NA"]
    unique_vals <- unique_vals[unique_vals != "NA"]

    for (i in seq_along(unique_vals)) {
      not_na_col <- not_na_cols[i]
      eval_quo(
        .df[, (not_na_col) := ifelse.(!!col == unique_vals[i], 1L, 0L, 0L)]
      )
    }

    # Since the prior step doesn't recognize NA as a character,
    # an extra step is needed to flag NA vals
    if (dummify_na) {

      na_col <- new_names[!new_names %in% not_na_cols]

      if (length(na_col) > 0) {
        eval_quo(
          .df[, (na_col) := as.integer(is.na(!!col))]
        )
      }
    }

    new_names <- f_sort(new_names)

    ordered_cols <- c(ordered_cols, new_names)
  }

  final_order <- c(original_cols, ordered_cols)

  setcolorder(.df, final_order)

  .df
}

globalVariables("where")
