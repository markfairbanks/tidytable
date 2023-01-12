#' Separate a character column into multiple columns
#'
#' @description
#' _Superseded_
#'
#' `separate()` has been superseded by `separate_wider_delim()`.
#'
#' Separates a single column into multiple columns using a user supplied separator or regex.
#'
#' If a separator is not supplied one will be automatically detected.
#'
#' Note: Using automatic detection or regex will be slower than simple separators
#' such as "," or ".".
#'
#' @param .df A data frame
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
#'   separate(x, into = c("c1", "c2"))
#'
#' # Faster if "sep" is provided
#' df %>%
#'   separate(x, into = c("c1", "c2"), sep = ".")
separate <- function(.df, col, into,
                     sep = "[^[:alnum:]]+",
                     remove = TRUE,
                     convert = FALSE,
                     ...) {
  separate.(
    .df, col = {{ col }}, into = into, sep = sep,
    remove = remove, convert = convert, ...
  )
}

#' @export
#' @keywords internal
#' @inherit separate
separate. <- function(.df, col, into,
                      sep = "[^[:alnum:]]+",
                      remove = TRUE,
                      convert = FALSE,
                      ...) {
  check_required(col)
  check_required(into)
  UseMethod("separate.")
}

#' @export
separate..tidytable <- function(.df, col, into,
                                sep = "[^[:alnum:]]+",
                                remove = TRUE,
                                convert = FALSE,
                                ...) {
  vec_assert(into, character())

  if (nchar(sep) == 1) {
    fixed <- TRUE
  } else {
    fixed <- FALSE
  }

  col <- tidyselect_names(.df, {{ col }})

  t_str_split <- tstrsplit(.df[[col]],
                           split = sep, fixed = fixed,
                           type.convert = convert)

  into_length <- length(into)
  split_length <- length(t_str_split)

  if (into_length > split_length) {
    extra <- into[(split_length + 1):into_length]
    into <- into[1:split_length]
  } else if (into_length < split_length) {
    t_str_split <- t_str_split[1:into_length]
    extra <- character()
  } else {
    extra <- character()
  }

  is_complete <- vec_detect_complete(into)

  into <- into[is_complete]

  t_str_split <- t_str_split[is_complete]

  out <- dt_j(.df, (into) := ..t_str_split)

  if (length(extra) > 0) {
    out <- dt_j(out, (extra) := NA_character_)
  }

  if (remove && col %notin% into) {
    out <- dt_j(out, (col) := NULL)
  }

  out
}

globalVariables("..t_str_split")

#' @export
separate..data.frame <- function(.df, col, into,
                                 sep = "[^[:alnum:]]+",
                                 remove = TRUE,
                                 convert = FALSE,
                                 ...) {
  .df <- as_tidytable(.df)
  separate(
    .df, col = {{ col }}, into = into, sep = sep,
    remove = remove, convert = convert, ...
  )
}

