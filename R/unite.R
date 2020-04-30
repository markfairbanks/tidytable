#' Unite multiple columns by pasting strings together
#'
#' @description
#' Convenience function to paste together multiple columns into one.
#'
#' @param .data A data.frame or data.table
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
#'   unite.("new_col", is.character)
#'
#' test_df %>%
#'   unite.("new_col", b, c, remove = FALSE)
#'
#' test_df %>%
#'   unite.("new_col", b, c, na.rm = TRUE)
#'
#' test_df %>%
#'   unite.()
unite. <- function(.data, col = "new_col", ..., sep = "_", remove = TRUE, na.rm = FALSE) {
  UseMethod("unite.")
}

#' @export
unite..data.frame <- function(.data, col = "new_col", ..., sep = "_", remove = TRUE, na.rm = FALSE) {

  .data <- as_tidytable(.data)

  dots <- enexprs(...)

  if (length(dots) == 0) {
    unite_cols <- names(.data)
  } else {
    unite_cols <- as.character(dots_selector(.data, ...))
  }

  col <- enexpr(col)

  if (na.rm) {
    unite_syms <- syms(unite_cols)

    middle_na <- paste0(sep, "NA", sep)
    start_na <- paste0("^NA", sep)
    end_na <- paste0(sep, "NA$")

    .data <- mutate.(.data, !!col := paste(!!!unite_syms, sep = !!sep) %>%
                       str_replace_all(!!middle_na, !!sep) %>%
                       str_replace_all(!!start_na, "") %>%
                       str_replace_all(!!end_na, ""))
  } else {
    .data <- shallow(.data)

    eval_expr(
      .data[, !!col := do.call(paste, c(.SD, sep = !!sep)), .SDcols = !!unite_cols]
    )
  }

  if (remove) .data <- .data[, -..unite_cols]

  .data
}

#' @export
#' @rdname unite.
dt_unite <- unite.
