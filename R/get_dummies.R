#' Convert character and factor columns to dummy variables
#'
#' @description
#' Convert character and factor columns to dummy variables
#'
#' Supports enhanced selection
#'
#' @param .data A data.frame or data.table
#' @param cols If NULL makes dummy variables for all character & factor columns. Or user can input a single column or a vector of unquoted columns to dummify
#' @param prefix TRUE/FALSE - If TRUE, a prefix will be added to new column names
#' @param prefix_sep Separator for new column names
#' @param drop_first TRUE/FALSE - If TRUE, the first dummy column will be dropped
#' @param dummify_na TRUE/FALSE - If TRUE, NAs will also get dummy columns
#'
#' @export
#'
#' @examples
#' test_df <- data.table::data.table(
#'   col1 = c(letters[1:3], NA),
#'   col2 = as.factor(c(letters[3:1], NA)),
#'   var1 = rnorm(7,0,1))
#'
#' # Automatically does all character/factor columns
#' test_df %>%
#'   dt_get_dummies()
#'
#' # Can select one column
#' test_df %>%
#'   dt_get_dummies(col1)
#'
#' # Can select one or multiple columns in a vector of unquoted column names
#' test_df %>%
#'   dt_get_dummies(c(col1, col2))
#'
#' test_df %>%
#'   dt_get_dummies(prefix_sep = ".", drop_first = TRUE)
#'
#' test_df %>%
#'   dt_get_dummies(c(col1, col2), dummify_na = FALSE)
dt_get_dummies <- function(.data,
                           cols = NULL,
                           prefix = TRUE,
                           prefix_sep = "_",
                           drop_first = FALSE,
                           dummify_na = TRUE) {
  UseMethod("dt_get_dummies")
}

#' @export
dt_get_dummies.data.frame <- function(.data,
                                      cols = NULL,
                                      prefix = TRUE,
                                      prefix_sep = "_",
                                      drop_first = FALSE,
                                      dummify_na = TRUE) {

  .data <- as_tidytable(.data)
  cols <- enexpr(cols)

  dt_get_dummies(.data, cols = !!cols,
                 prefix = prefix, prefix_sep = prefix_sep,
                 drop_first = drop_first, dummify_na = dummify_na)
}

#' @export
dt_get_dummies.tidytable <- function(.data,
                                      cols = NULL,
                                      prefix = TRUE,
                                      prefix_sep = "_",
                                      drop_first = FALSE,
                                      dummify_na = TRUE) {

  .data <- shallow(.data)
  cols <- enexpr(cols)

  if (is.null(cols)) {
    # If NULL, select all character & factor cols
    data_names <- colnames(.data)

    chr_cols <- data_names[dt_map_lgl(.data, is.character)]
    fct_cols <- data_names[dt_map_lgl(.data, is.factor)]

    cols <- syms(c(chr_cols, fct_cols))
  } else {
    cols <- vec_selector(.data, !!cols)
  }

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

    for (i in seq_along(unique_vals)) {
      eval_tidy(expr(
        .data[!!col == unique_vals[i], new_names[i] := 1L][]
      ))
    }

    # Since the prior step doesn't recognize NA as a character,
    # an extra step is needed to flag NA vals
    if (dummify_na) {

      na_col <- new_names[str_detect(new_names, "NA")]

      eval_tidy(expr(
        .data[is.na(!!col), (na_col) := 1][]
      ))
    }
  }
  .data
}
