#' Rename multiple columns
#'
#' @description
#' Rename multiple columns with the same transformation
#'
#' @param .df A data.table or data.frame
#' @param .fn Function to transform the names with.
#' @param .cols Columns to rename. Defaults to all columns. `tidyselect` compatible.
#' @param ... Other parameters to pass to the function
#'
#' @export
#' @md
#'
#' @examples
#' test_df <- data.table(
#'   x = 1,
#'   y = 2,
#'   double_x = 2,
#'   double_y = 4
#' )
#'
#' test_df %>%
#'   rename_with.(toupper)
#'
#' test_df %>%
#'   rename_with.(~ toupper(.x))
#'
#' test_df %>%
#'   rename_with.(~ toupper(.x), .cols = c(x, double_x))
rename_with. <- function(.df, .fn, .cols = everything(), ...) {
  UseMethod("rename_with.")
}

#' @export
rename_with..data.frame <- function(.df, .fn, .cols = everything(), ...) {
  .df <- as_tidytable(.df)

  .cols <- select_vec_chr(.df, {{ .cols }})

  if (length(.cols) == 0) return(.df)

  .df <- shallow(.df)

  .fn <- as_function(.fn)

  new_names <- .fn(.cols, ...)

  setnames(.df, .cols, new_names)

  .df
}
