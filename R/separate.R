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
#' @param sep Separator to split on. Can be specified or detected automatically
#' @param remove If TRUE, remove the input column from the output data.table
#' @param convert TRUE calls `type.convert()` with `as.is = TRUE` on new columns
#' @param ... Arguments passed on to methods
#'
#' @export
#'
#' @examples
#' test_df <- data.table(x = c("a", "a.b", "a.b", NA))
#'
#' # "sep" can be automatically detected (slower)
#' test_df %>%
#'   separate.(x, into = c("c1", "c2"))
#'
#' # Faster if "sep" is provided
#' test_df %>%
#'   separate.(x, into = c("c1", "c2"), sep = ".")
separate. <- function(.df, col, into,
                      sep = "[^[:alnum:]]+",
                      remove = TRUE,
                      convert = FALSE,
                      ...) {
  UseMethod("separate.")
}

#' @export
separate..data.frame <- function(.df, col, into,
                                 sep = "[^[:alnum:]]+",
                                 remove = TRUE,
                                 convert = FALSE,
                                 ...) {
  .df <- as_tidytable(.df)
  .df <- shallow(.df)

  vec_assert(into, character())

  if (missing(col)) abort("col is missing and must be supplied")
  if (missing(into)) abort("into is missing and must be supplied")

  if (nchar(sep) == 1) {
    fixed <- TRUE
  } else {
    fixed <- FALSE
  }

  col <- enquo(col)

  eval_quo(
    .df[, (into) := tstrsplit(!!col, split = sep, fixed = fixed, type.convert = convert)]
  )

  if (remove) .df <- mutate.(.df, !!col := NULL)

  .df[]
}
