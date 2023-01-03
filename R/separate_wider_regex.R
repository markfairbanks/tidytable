#' Separate a character column into multiple columns using regex patterns
#'
#' @description
#' Separate a character column into multiple columns using regex patterns
#'
#' @param .df A data frame
#' @param cols Columns to separate
#' @param patterns patterns
#' @inheritParams rlang::args_dots_empty
#' @param names_sep Names separator
#' @param names_repair Treatment of duplicate names. See `?vctrs::vec_as_names` for options/details.
#' @param too_few What to do when too few column names are supplied
#' @param cols_remove Should old columns be removed
#'
#' @export
#'
#' @examples
#' df <- tidytable(id = 1:3, x = c("m-123", "f-455", "f-123"))
#'
#' df %>%
#'   separate_wider_regex(x, c(gender = ".", ".", unit = "\\d+"))
separate_wider_regex <- function(.df,
                                 cols,
                                 patterns,
                                 ...,
                                 names_sep = NULL,
                                 names_repair = "check_unique",
                                 too_few = "error",
                                 cols_remove = TRUE) {
  has_name <- have_name(patterns)
  groups <- str_c("(", if_else(has_name, "", "?:"), patterns, ")")
  full_match <- str_c("^", str_flatten(groups), "$")

  too_few <- arg_match(too_few)

  cols <- tidyselect_names(.df, {{ cols }})

  names <- names(patterns)[has_name]

  for (col in cols) {
    if (!is.null(names_sep)) {
      names_sep <- names_sep %||% "_"
      into <- str_c(col, names, sep = names_sep)
    } else if (length(cols) > 1) {
      abort("`names_sep` must be provided when `length(cols) > 0`")
    } else {
      into <- names
    }
    .df <- extract(.df, all_of(col), into, full_match, cols_remove)
  }

  .df
}
