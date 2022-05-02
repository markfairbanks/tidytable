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
#' df <- data.table(
#'   x = 1,
#'   y = 2,
#'   double_x = 2,
#'   double_y = 4
#' )
#'
#' df %>%
#'   rename_with.(toupper)
#'
#' df %>%
#'   rename_with.(~ toupper(.x))
#'
#' df %>%
#'   rename_with.(~ toupper(.x), .cols = c(x, double_x))
rename_with. <- function(.df, .fn = NULL, .cols = everything(), ...) {
  UseMethod("rename_with.")
}

#' @export
rename_with..tidytable <- function(.df, .fn = NULL, .cols = everything(), ...) {
  if (is.null(.fn)) return(.df)

  .cols <- tidyselect_names(.df, {{ .cols }})

  if (length(.cols) == 0) return(.df)

  .fn <- as_function(.fn)

  new_names <- .fn(.cols, ...)

  .df <- df_set_names(.df, new_names, .cols)

  .df
}

#' @export
rename_with..data.frame <- function(.df, .fn = NULL, .cols = everything(), ...) {
  .df <- as_tidytable(.df)
  rename_with.(.df, .fn, {{ .cols }}, ...)
}
