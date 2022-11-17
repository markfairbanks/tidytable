#' Separate a character column into multiple columns
#'
#' @description
#' Separates a single column into multiple columns
#'
#' @param .df A data frame
#' @param cols Columns to separate
#' @param delim Delimiter to separate on
#' @inheritParams rlang::args_dots_empty
#' @param names New column names to separate into
#' @param names_sep Names separator
#' @param names_repair Treatment of duplicate names. See `?vctrs::vec_as_names` for options/details.
#' @param too_few What to do when too few column names are supplied
#' @param too_many What to do when too many column names are supplied
#' @param cols_remove Should old columns be removed
#'
#' @export
#'
#' @examples
#' df <- tidytable(x = c("a", "a_b", "a_b", NA))
#'
#' df %>%
#'   separate_wider_delim(x, delim = "_", names = c("left", "right"))
#'
#' df %>%
#'   separate_wider_delim(x, delim = "_", names_sep = "")
separate_wider_delim <- function(.df,
                                 cols,
                                 delim,
                                 ...,
                                 names = NULL,
                                 names_sep = NULL,
                                 names_repair = "check_unique",
                                 too_few = c("align_start", "error"),
                                 too_many = c("drop", "error"),
                                 cols_remove = TRUE) {
  check_required(cols)
  check_required(delim)

  cols <- tidyselect_names(.df, {{ cols }})

  if (length(cols) > 1 && is.null(names_sep)) {
    abort("`names_sep` must be provided when multiple columns are provided")
  }

  for (col in cols) {
    t_str_split <- tstrsplit(.df[[col]], split = delim, fixed = TRUE)

    split_length <- length(t_str_split)

    names_null <- is.null(names)

    if (names_null) {
      names <- as.character(seq_len(split_length))
    }

    names_length <- length(names)

    if (is.null(names_sep) && names_null) {
      names_sep <- ""
    }

    if (!is.null(names_sep)) {
      names <- paste(col, names, sep = names_sep)
    }

    if (names_length < split_length) {
      too_few <- arg_match0(too_few, c("align_start", "error"))
      if (too_few == "error") {
        abort("Not enough column names supplied")
      }
      t_str_split <- t_str_split[1:names_length]
      extra <- character()
    } else if (names_length > split_length) {
      too_many <- arg_match0(too_many, c("drop", "error"))
      if (too_many == "error") {
        abort("Too many column names supplied")
      }
      extra <- names[(split_length + 1):names_length]
      names <- names[1:split_length]
    } else {
      extra <- character()
    }

    is_complete <- vec_detect_complete(names)

    names <- names[is_complete]

    t_str_split <- t_str_split[is_complete]

    out <- dt_j(.df, (names) := ..t_str_split)

    if (length(extra) > 0) {
      out <- dt_j(out, (extra) := NA_character_)
    }

    if (cols_remove && col %notin% names) {
      out <- dt_j(out, (col) := NULL)
    }
  }

  df_name_repair(out, names_repair)
}
