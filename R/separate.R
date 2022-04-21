#' Separate a character column into multiple columns
#'
#' @description
#' Separates a single column into multiple columns using a user supplied separator or regex.
#'
#' If a separator is not supplied one will be automatically detected.
#'
#' Note: Using automatic detection or regex will be slower than simple separators such as "," or ".".
#'
#' @param .df A data.frame or data.table
#' @param col The column to split into multiple columns
#' @param into New column names to split into. A character vector.
#' Use `NA` to omit the variable in the output.
#' @param sep Separator to split on. Can be specified or detected automatically
#' @param remove If TRUE, remove the input column from the output data.table
#' @param convert TRUE calls `type.convert()` with `as.is = TRUE` on new columns
#' @param ... Arguments passed on to methods
#'
#' @export
#'
#' @examples
#' df <- data.table(x = c("a", "a.b", "a.b", NA))
#'
#' # "sep" can be automatically detected (slower)
#' df %>%
#'   separate.(x, into = c("c1", "c2"))
#'
#' # Faster if "sep" is provided
#' df %>%
#'   separate.(x, into = c("c1", "c2"), sep = ".")
separate. <- function(.df, col, into,
                      sep = "[^[:alnum:]]+",
                      remove = TRUE,
                      convert = FALSE,
                      ...) {
  UseMethod("separate.")
}

#' @export
separate..tidytable <- function(.df, col, into,
                                sep = "[^[:alnum:]]+",
                                remove = TRUE,
                                convert = FALSE,
                                ...) {
  if (missing(col)) abort("col is missing and must be supplied")
  if (missing(into)) abort("into is missing and must be supplied")

  vec_assert(into, character())

  if (nchar(sep) == 1) {
    fixed <- TRUE
  } else {
    fixed <- FALSE
  }

  col <- enquo(col)

  col <- tidyselect_syms(.df, !!col)[[1]]

  not_na_into <- !is.na(into)

  keep <- seq_along(into)[not_na_into]
  into <- into[not_na_into]

  t_str_split <- quo_squash(call2(
    "tstrsplit", col, split = sep, fixed = fixed,
    keep = keep, type.convert = convert,
    .ns = "data.table"
  ))

  .df <- dt_j(.df, (into) := !!t_str_split)

  if (remove) {
    .df <- dt_j(.df, !!col := NULL)
  }

  .df[]
}

#' @export
separate..data.frame <- function(.df, col, into,
                                 sep = "[^[:alnum:]]+",
                                 remove = TRUE,
                                 convert = FALSE,
                                 ...) {
  .df <- as_tidytable(.df)
  separate.(
    .df, col = {{ col }}, into = into, sep = sep,
    remove = remove, convert = convert, ...
  )
}
