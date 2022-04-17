#' Separate a collapsed column into multiple rows
#'
#' @description
#' If a column contains observations with multiple delimited values, separate them each into their own row.
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
#' separate_rows.(df, y, z)
#'
#' separate_rows.(df, y, z, convert = TRUE)
separate_rows. <- function(.df, ..., sep = "[^[:alnum:].]+", convert = FALSE) {
  UseMethod("separate_rows.")
}

#' @export
separate_rows..tidytable <- function(.df, ..., sep = "[^[:alnum:].]+", convert = FALSE) {
  vec_assert(sep, character(), 1)
  vec_assert(convert, logical(), 1)

  col_order <- copy(names(.df))

  dots <- enquos(...)
  if (length(dots) == 0) return(.df)

  dt_env <- get_dt_env(dots)

  cols <- tidyselect_syms(.df, !!!dots)

  .df <- mutate.(.df, .separate_id = .I)

  col_names <- as.character(cols)
  other_col_names <- setdiff(names(.df), col_names)

  if (nchar(sep) == 1) {
    fixed_bool <- TRUE
  } else {
    fixed_bool <- FALSE
  }

  # Create a list of calls to strsplit()
  # Can't use a for loop - all columns selected must be in the same data.table call
  # Reason: Using df %>% separate_rows(col1) %>% separate_rows(col2)
  # is different than df %>% separate_rows(col1, col2)
  split_calls <- map.(cols, ~ call2('strsplit', .x, split = sep, fixed = fixed_bool))
  names(split_calls) <- col_names

  j <- expr(c(!!!split_calls))

  dt_expr <- call2_j(.df, j, .by = other_col_names)

  .df <- eval_tidy(dt_expr, env = dt_env)

  .df <- mutate.(.df, .separate_id = NULL)

  setcolorder(.df, col_order)

  if (convert) {
    .df <- mutate.(.df, across.(all_of(col_names), ~ type.convert(.x, as.is = TRUE)))
  }

  .df[]
}

#' @export
separate_rows..data.frame <- function(.df, ..., sep = "[^[:alnum:].]+", convert = FALSE) {
  .df <- as_tidytable(.df)
  separate_rows.(.df, ..., sep = sep, convert = convert)
}

globalVariables(".id")
