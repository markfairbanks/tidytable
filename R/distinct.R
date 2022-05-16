#' Select distinct/unique rows
#'
#' @description
#' Retain only unique/distinct rows from an input df.
#'
#' @param .df A data.frame or data.table
#' @param ... Columns to select before determining uniqueness. If omitted, will use all columns.
#' `tidyselect` compatible.
#' @param .keep_all Only relevant if columns are provided to ... arg.
#' This keeps all columns, but only keeps the first row of each distinct
#' values of columns provided to ... arg.
#'
#' @export
#'
#' @examples
#' df <- tidytable(
#'   x = 1:3,
#'   y = 4:6,
#'   z = c("a", "a", "b")
#' )
#'
#' df %>%
#'   distinct.()
#'
#' df %>%
#'   distinct.(z)
distinct. <- function(.df, ..., .keep_all = FALSE) {
  UseMethod("distinct.")
}

#' @export
distinct..tidytable <- function(.df, ..., .keep_all = FALSE) {
  vec_assert(.keep_all, logical(), 1)

  dots <- enquos(...)

  across_check(dots)

  if (length(dots) == 0) {
    out <- unique(.df)
  } else {
    cols <- tidyselect_locs(.df, ...)

    out <- unique(.df, by = cols)

    if (!.keep_all) {
      cols_keep <- unname(cols)
      out <- select.(out, any_of(cols_keep))
    }

    named_bool <- have_name(dots)

    if (any(named_bool)) {
      named_dots <- dots[named_bool]

      out <- rename.(out, !!!named_dots)
    }
  }

  out
}

#' @export
distinct..data.frame <- function(.df, ..., .keep_all = FALSE) {
  .df <- as_tidytable(.df)
  distinct.(.df, ..., .keep_all = .keep_all)
}

across_check <- function(dots) {
  use_across <- map_lgl.(dots, quo_is_call, "across.")

  if (any(use_across)) {
    abort(
      paste0(
        c("across.() is unnecessary in distinct.()",
        "Please directly use tidyselect:",
        "Ex: df %>% distinct.(starts_with('x'))"),
        collapse = "\n"
      )
    )
  }
}
