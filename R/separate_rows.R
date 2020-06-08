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
#' test_df <- data.table(
#'   x = 1:3,
#'   y = c("a", "d,e,f", "g,h"),
#'   z = c("1", "2,3,4", "5,6")
#' )
#'
#' separate_rows.(test_df, y, z)
#'
#' separate_rows.(test_df, y, z, convert = TRUE)
separate_rows. <- function(.df, ..., sep = "[^[:alnum:].]+", convert = FALSE) {
  UseMethod("separate_rows.")
}

#' @export
separate_rows..data.frame <- function(.df, ..., sep = "[^[:alnum:].]+", convert = FALSE) {

  .df <- as_tidytable(.df)

  vec_assert(sep, character(), 1)
  vec_assert(convert, logical(), 1)

  col_order <- names(.df)

  .df <- shallow(.df)

  cols <- select_dots_sym(.df, ...)

  .df[ , .id := .I]

  col_names <- as.character(cols)
  other_col_names <- setdiff(names(.df), col_names)

  if (nchar(sep) == 1) fixed_flag <- TRUE
  else fixed_flag <- FALSE

  # Create a list of calls to strsplit()
  # Can't use a for loop - all columns selected must be in the same data.table call
  # Reason: Using df %>% separate_rows(col1) %>% separate_rows(col2)
  # is different than df %>% separate_rows(col1, col2)
  split_calls <- map.(cols, ~ call2('strsplit', .x, split = sep, fixed = fixed_flag))
  names(split_calls) <- col_names

  .df <- eval_quo(
    .df[, c(!!!split_calls), by = other_col_names]
  )

  .df[ , .id := NULL]

  setcolorder(.df, col_order)

  if (convert) .df <- mutate_across.(.df, c(!!!cols), type.convert, as.is = TRUE)

  .df[]
}
