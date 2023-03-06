#' Separate a collapsed column into multiple rows
#'
#' @description
#' _Superseded_
#'
#' `separate_rows()` has been superseded by `separate_longer_delim()`.
#'
#' If a column contains observations with multiple delimited values,
#' separate them each into their own row.
#'
#' @param .df A data.frame or data.table
#' @param ... Columns to separate across multiple rows. `tidyselect` compatible
#' @param sep Separator delimiting collapsed values
#' @param convert If TRUE, runs `type.convert()` on the resulting column.
#' Useful if the resulting column should be type integer/double.
#'
#' @export
#'
#' @examples
#' df <- data.table(
#'   x = 1:3,
#'   y = c("a", "d,e,f", "g,h"),
#'   z = c("1", "2,3,4", "5,6")
#' )
#'
#' separate_rows(df, y, z)
#'
#' separate_rows(df, y, z, convert = TRUE)
separate_rows <- function(.df, ..., sep = "[^[:alnum:].]+", convert = FALSE) {
  .df <- .df_as_tidytable(.df)

  col_order <- names(.df)

  dots <- enquos(...)
  if (length(dots) == 0) return(.df)

  cols <- tidyselect_syms(.df, !!!dots)

  # If all cols are being selected, need to create a `.separate_id` column
  needs_separate_id <- length(cols) == ncol(.df)
  if (needs_separate_id) {
    .df <- dt_j(.df, .separate_id := .I)
  }

  col_names <- as.character(cols)
  .other_col_names <- setdiff(names(.df), col_names)

  if (nchar(sep) == 1) {
    fixed_bool <- TRUE
  } else {
    fixed_bool <- FALSE
  }

  # Create a list of calls to strsplit()
  # Can't use a for loop - all columns selected must be in the same data.table call
  # Reason: Using df %>% separate_rows(col1) %>% separate_rows(col2)
  # is different than df %>% separate_rows(col1, col2)
  split_calls <- map(cols, ~ call2('strsplit', .x, split = sep, fixed = fixed_bool))
  names(split_calls) <- col_names

  j <- expr(c(!!!split_calls))

  out <- dt_j(.df, !!j, by = .other_col_names)

  if (needs_separate_id) {
    out <- dt_j(out, .separate_id := NULL)
  }

  out <- df_col_order(out, col_order)

  if (convert) {
    out <- mutate(out, across(all_of(col_names), ~ type.convert(.x, as.is = TRUE)))
  }

  out
}

#' @export
#' @keywords internal
#' @inherit separate_rows
separate_rows. <- function(.df, ..., sep = "[^[:alnum:].]+", convert = FALSE) {
  deprecate_dot_fun()
  separate_rows(.df, ..., sep = sep, convert = convert)
}

globalVariables(".separate_id")
