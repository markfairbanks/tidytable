#' Unnest a list-column of vectors into a wide data frame
#'
#' @param .df A data.table or data.frame
#' @param col Column to unnest
#' @param names_sep If `NULL`, the default, the names will be left as they are.
#'   If a string, the inner and outer names will be pasted together with `names_sep`
#'   as the separator.
#' @param simplify Currently not supported. Errors if not `NULL`.
#' @param names_repair Treatment of duplicate names. See `?vctrs::vec_as_names` for options/details.
#' @param ptype Optionally a named list of ptypes declaring the desired output type of each component.
#' @param transform Optionally a named list of transformation functions applied to each component.
#'
#' @export
#'
#' @examples
#' df <- tidytable(
#'   x = 1:3,
#'   y = list(0, 1:3, 4:5)
#' )
#'
#' # Automatically creates names
#' df %>% unnest_wider.(y)
#'
#' # But you can provide names_sep for increased naming control
#' df %>% unnest_wider.(y, names_sep = "_")
unnest_wider. <- function(.df, col, names_sep = NULL,
                          simplify = NULL, names_repair = "check_unique",
                          ptype = list(), transform = list()) {
  UseMethod("unnest_wider.")
}

#' @export
unnest_wider..tidytable <- function(.df, col, names_sep = NULL,
                                    simplify = NULL, names_repair = "check_unique",
                                    ptype = list(), transform = list()) {
  .col <- enquo(col)

  .l <- pull.(.df, !!.col)

  if (!is.atomic(.l[[1]])) {
    abort("Only vectors are currently supported")
  }

  if (!is.null(simplify)) {
    abort("The simplify argument is not currently supported")
  }

  .l <- map.(.l, ~ unnest_wider_tidytable(!!!.x))

  out <- bind_rows.(.l)

  if (!is.null(names_sep)) {
    out_names <- names(out)
    new_names <- paste(as_name(.col), out_names, sep = names_sep)

    setnames(out, out_names, new_names)
  } else {
    out <- df_name_repair(out, .name_repair = "universal")
  }

  .df <- mutate.(.df, !!.col := NULL)

  if (ncol(.df) > 0) {
    out <- bind_cols.(.df, out, .name_repair = names_repair)
  }

  out <- change_types(out, names(out), ptype, "ptypes")
  out <- change_types(out, names(out), transform, "transform")

  out
}

#' @export
unnest_wider..data.frame <- function(.df, col, names_sep = NULL,
                                     simplify = NULL, names_repair = "check_unique",
                                     ptype = list(), transform = list()) {
  .df <- as_tidytable(.df)
  unnest_wider.(
    .df, col = {{ col }}, names_sep = names_sep, simplify = simplify,
    names_repair = names_repair, ptype = ptype, transform = transform
  )
}

unnest_wider_tidytable <- function(...) {
  dots <- list2(...)
  names(dots) <- names(dots) %||% seq_along(dots)

  new_tidytable(dots)
}
