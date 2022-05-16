#' Unite multiple columns by pasting strings together
#'
#' @description
#' Convenience function to paste together multiple columns into one.
#'
#' @param .df A data.frame or data.table
#' @param col Name of the new column, as a string.
#' @param ... Selection of columns. If empty all variables are selected.
#' `tidyselect` compatible.
#' @param sep Separator to use between values
#' @param remove If TRUE, removes input columns from the data.table.
#' @param na.rm If TRUE, NA values will be not be part of the concatenation
#'
#' @export
#'
#' @examples
#' df <- tidytable(
#'     a = c("a", "a", "a"),
#'     b = c("b", "b", "b"),
#'     c = c("c", "c", NA)
#' )
#'
#' df %>%
#'   unite.("new_col", b, c)
#'
#' df %>%
#'   unite.("new_col", where(is.character))
#'
#' df %>%
#'   unite.("new_col", b, c, remove = FALSE)
#'
#' df %>%
#'   unite.("new_col", b, c, na.rm = TRUE)
#'
#' df %>%
#'   unite.()
unite. <- function(.df, col = ".united", ..., sep = "_", remove = TRUE, na.rm = FALSE) {
  UseMethod("unite.")
}

#' @export
unite..tidytable <- function(.df, col = ".united", ..., sep = "_", remove = TRUE, na.rm = FALSE) {
  col <- as_name(enquo(col))

  dots <- enquos(...)
  if (length(dots) == 0) {
    unite_cols <- names(.df)
    locs <- seq_along(unite_cols)
  } else {
    locs <- tidyselect_locs(.df, !!!dots)
    unite_cols <- names(.df)[locs]
  }

  if (is_true(na.rm)) {
    cols <- unname(map.(select.(.df, any_of(unite_cols)), as.character))
    rows <- transpose(cols)

    .united <- map_chr.(rows, ~ paste0(.x[!is.na(.x)], collapse = sep))

    out <- mutate.(.df, !!col := .env$.united)
  } else {
    out <- mutate.(.df, !!col := paste(!!!syms(unite_cols), sep = sep))
  }

  out <- relocate.(out, !!col, .before = min(locs))

  if (is_true(remove)) {
    drop_cols <- setdiff(unite_cols, col)
    out <- select.(out, -any_of(drop_cols))
  }

  out
}

#' @export
unite..data.frame <- function(.df, col = ".united", ..., sep = "_", remove = TRUE, na.rm = FALSE) {
  .df <- as_tidytable(.df)
  unite.(.df, {{ col }}, ..., sep = sep, remove = remove, na.rm = na.rm)
}
