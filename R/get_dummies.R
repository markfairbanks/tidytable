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

  original_cols <- names(.df)

  ordered_cols <- character()

  df_list <- vector("list", length(cols))

  for (i in seq_along(cols)) {
    col <- cols[[i]]
    col_name <- as.character(col)
    .vals <- .df[[col_name]]

    if (!is.character(.vals)) {
      .vals <- as.character(.vals)
    }

    if (dummify_na) {
      .vals <- .vals %|% "NA"
    } else {
      .vals <- .vals %|% "...NA..."
    }

    if (prefix) {
      col_prefix <- paste0(col_name, prefix_sep)
    } else {
      col_prefix <- ""
    }

    dummy_df <- model.matrix(~ .vals - 1)
    df_names <- colnames(dummy_df)
    dummy_df <- map.(seq_len(ncol(dummy_df)), ~ unname(dummy_df[, .x]))
    names(dummy_df) <- df_names
    dummy_df <- new_tidytable(dummy_df)

    df_names <- names(dummy_df)
    df_names <- str_replace.(df_names, "^.vals", col_prefix)
    dummy_df <- set_names(dummy_df, df_names)

    if (!dummify_na) {
      na_col <- df_names[endsWith(df_names, "...NA...")]
      if (length(na_col) > 0) {
        dummy_df[, (na_col) := NULL]
        df_names <- names(dummy_df)
      }
    }

    if (drop_first) {
      first_col <- df_names[as.logical(vec_slice(dummy_df, 1))]
      dummy_df[, (first_col) := NULL]
      df_names <- names(dummy_df)
    }

    df_list[[i]] <- dummy_df

    sorted_names <- f_sort(df_names)

    ordered_cols <- c(ordered_cols, sorted_names)
  }

  final_order <- c(original_cols, ordered_cols)

  out <- bind_cols.(.df, df_list)

  setcolorder(out, final_order)

  out
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
