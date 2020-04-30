#' Convert character and factor columns to dummy variables
#'
#' @description
#' Convert character and factor columns to dummy variables
#'
#' @param .data A data.frame or data.table
#' @param cols A single column or a vector of unquoted columns to dummify.
#' Defaults to all character & factor columns using `c(is.character, is.factor)`.
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
#'   get_dummies.(c(is.character, -col2))
#'
#' test_df %>%
#'   get_dummies.(prefix_sep = ".", drop_first = TRUE)
#'
#' test_df %>%
#'   get_dummies.(c(col1, col2), dummify_na = FALSE)
get_dummies. <- function(.data,
                         cols = c(is.character, is.factor),
                         prefix = TRUE,
                         prefix_sep = "_",
                         drop_first = FALSE,
                         dummify_na = TRUE) {
  UseMethod("get_dummies.")
}

#' @export
get_dummies..data.frame <- function(.data,
                                    cols = c(is.character, is.factor),
                                    prefix = TRUE,
                                    prefix_sep = "_",
                                    drop_first = FALSE,
                                    dummify_na = TRUE) {

  .data <- as_tidytable(.data)
  .data <- shallow(.data)

  cols <- enexpr(cols)

  cols <- vec_selector(.data, !!cols)

  for (col in cols) {

    col_name <- as.character(col)

    if (drop_first) unique_vals <- unique(as.character(.data[[col_name]]))[-1]
    else unique_vals <- unique(as.character(.data[[col_name]]))

    # If NAs need dummies, convert to character string "NA" for col name creation
    if (dummify_na) unique_vals <- fifelse(is.na(unique_vals), "NA", unique_vals)
    else unique_vals <- unique_vals[!is.na(unique_vals)]

    if (prefix) new_names <- str_c(col_name, unique_vals, sep = prefix_sep)
    else new_names <- unique_vals

    .data[, (new_names) := 0]

    # Remove "NA" from unique vals after new_names columns are made
    not_na_cols <- new_names[unique_vals != "NA"]
    unique_vals <- unique_vals[unique_vals != "NA"]

    for (i in seq_along(unique_vals)) {
      eval_expr(
        .data[!!col == unique_vals[i], not_na_cols[i] := 1L]
      )
    }

    # Since the prior step doesn't recognize NA as a character,
    # an extra step is needed to flag NA vals
    if (dummify_na) {

      na_col <- new_names[!new_names %in% not_na_cols]

      eval_expr(
        .data[is.na(!!col), (na_col) := 1]
      )
    }
  }
  .data[]
}

#' @export
#' @rdname get_dummies.
dt_get_dummies <- get_dummies.
