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
#' @md
#'
#' @examples
#' test_df <- tidytable(a = c("a", "a", "a"),
#'                      b = c("b", "b", "b"),
#'                      c = c("c", "c", NA))
#'
#' test_df %>%
#'   unite.("new_col", b, c)
#'
#' test_df %>%
#'   unite.("new_col", where(is.character))
#'
#' test_df %>%
#'   unite.("new_col", b, c, remove = FALSE)
#'
#' test_df %>%
#'   unite.("new_col", b, c, na.rm = TRUE)
#'
#' test_df %>%
#'   unite.()
unite. <- function(.df, col = "new_col", ..., sep = "_", remove = TRUE, na.rm = FALSE) {
  UseMethod("unite.")
}

#' @export
unite..data.frame <- function(.df, col = "new_col", ..., sep = "_", remove = TRUE, na.rm = FALSE) {

  .df <- as_tidytable(.df)

  vec_assert(sep, character(), 1)
  vec_assert(remove, logical(), 1)
  vec_assert(na.rm, logical(), 1)

  dots <- enquos(...)

  if (length(dots) == 0) {
    unite_cols <- names(.df)
  } else {
    unite_cols <- select_dots_chr(.df, ...)
  }

  if (na.rm) {
    unite_syms <- syms(unite_cols)

    middle_na <- paste0(sep, "NA", sep)
    start_na <- paste0("^NA", sep)
    end_na <- paste0(sep, "NA$")

    .df <- mutate.(.df, !!col := paste(!!!unite_syms, sep = sep) %>%
                     str_replace_all.(middle_na, sep) %>%
                     str_replace_all.(start_na, "") %>%
                     str_replace_all.(end_na, ""))
  } else {
    .df <- shallow(.df)

    eval_quo(
      .df[, !!col := do.call(paste, c(.SD, sep = sep)), .SDcols = !!unite_cols]
    )
  }

  if (remove) .df <- .df[, -..unite_cols]

  .df
}

#' @export
#' @rdname dt_verb
#' @inheritParams unite.
dt_unite <- function(.df, col = "new_col", ..., sep = "_", remove = TRUE, na.rm = FALSE) {
  deprecate_warn("0.5.2", "tidytable::dt_unite()", "unite.()")

  unite.(.df, col = col, ..., sep = sep, remove = remove, na.rm = na.rm)
}
