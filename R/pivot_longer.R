#' Pivot data from wide to long
#'
#'
#' @description
#' `pivot_longer.()` "lengthens" the data, increasing the number of rows and decreasing
#' the number of columns.
#'
#' @param .df The data table to pivot longer
#' @param cols Vector of bare column names. Can add/drop columns. `tidyselect` compatible.
#' @param names_to Name of the new "names" column. Must be a string.
#' @param values_to Name of the new "values" column. Must be a string.
#' @param names_sep If `names_to` contains multiple values, `names_sep` takes
#' the same specification as `separate.()`.
#' @param names_pattern If `names_to` contains multiple values, `names_pattern` takes
#' the same specification as `extract.()`, a regular expression containing matching groups.
#' @param names_ptypes,values_ptypes A list of column name-prototype pairs. See ``?vctrs::`theory-faq-coercion```
#' for more info on vctrs coercion.
#' @param names_transform,values_transform A list of column name-function pairs. Use these arguments
#' if you need to change the types of specific columns.
#' @param names_repair Treatment of duplicate names. See `?vctrs::vec_as_names` for options/details.
#' @param values_drop_na If TRUE, rows will be dropped that contain NAs.
#' @param fast_pivot _experimental_: Fast pivoting. If `TRUE`, the `names_to` column will be returned as a `factor`,
#' otherwise it will be a `character` column. Defaults to `FALSE` to match tidyverse semantics.
#' @param ... Additional arguments to passed on to methods.
#'
#' @export
#' @md
#'
#' @examples
#' test_df <- data.table(
#'   x = 1:3,
#'   y = 4:6,
#'   z = c("a", "b", "c")
#' )
#'
#' test_df %>%
#'   pivot_longer.(c(x, y))
#'
#' test_df %>%
#'   pivot_longer.(cols = -z, names_to = "stuff", values_to = "things")
pivot_longer. <- function(.df,
                          cols = everything(),
                          names_to = "name",
                          values_to = "value",
                          names_sep = NULL,
                          names_pattern = NULL,
                          names_ptypes = list(),
                          names_transform = list(),
                          names_repair = "check_unique",
                          values_drop_na = FALSE,
                          values_ptypes = list(),
                          values_transform = list(),
                          fast_pivot = FALSE,
                          ...) {
  UseMethod("pivot_longer.")
}

#' @export
pivot_longer..data.frame <- function(.df,
                                     cols = everything(),
                                     names_to = "name",
                                     values_to = "value",
                                     names_sep = NULL,
                                     names_pattern = NULL,
                                     names_ptypes = list(),
                                     names_transform = list(),
                                     names_repair = "check_unique",
                                     values_drop_na = FALSE,
                                     values_ptypes = list(),
                                     values_transform = list(),
                                     fast_pivot = FALSE,
                                     ...) {

  .df <- as_tidytable(.df)

  names <- names(.df)

  cols <- select_vec_chr(.df, {{ cols }})

  if (length(cols) == 0) abort("At least one column must be supplied to cols")

  id_vars <- names[!names %in% cols]

  multiple_names_to <- length(names_to) > 1

  if (multiple_names_to) {

    null_sep <- is.null(names_sep)
    null_pattern <- is.null(names_pattern)

    if (null_sep && null_pattern)
      abort("If you supply multiple names in `names_to` you must also
            supply `names_sep` or `names_pattern`")

    if (!null_sep && !null_pattern)
      abort("only one of names_sep or names_pattern should be provided")

    var_name <- str_c.(names_to, collapse = "___")
  } else {
    var_name <- names_to
  }

  .df <- melt(
    data = .df,
    id.vars = id_vars,
    measure.vars = cols,
    variable.name = var_name,
    value.name = values_to,
    ...,
    na.rm = values_drop_na,
    variable.factor = fast_pivot,
    value.factor = FALSE
  )

  if (multiple_names_to) {

    if (!null_sep) {
      .df <- separate.(.df, !!sym(var_name), into = names_to, sep = names_sep)
    } else {
      .df <- extract.(.df, !!sym(var_name), into = names_to, regex = names_pattern)
    }

    # Put new names before value column
    .df <- relocate.(.df, !!!syms(names_to), .before = !!sym(values_to))
  }

  .df <- df_name_repair(.df, .name_repair = names_repair)

  ## names_ptype & names_transform
  # optionally, cast variables generated from columns
  cast_cols <- intersect(names_to, names(names_ptypes))
  for (col in cast_cols) {
    .df[[col]] <- vec_cast(.df[[col]], names_ptypes[[col]])
  }

  # transform cols
  coerce_cols <- intersect(names_to, names(names_transform))
  for (col in coerce_cols) {
    f <- as_function(names_transform[[col]])
    .df[[col]] <- f(.df[[col]])
  }

  ## values_ptype & values_transform
  # optionally, cast variables generated from columns
  cast_col <- intersect(values_to, names(values_ptypes))
  if (length(cast_col) > 0) {
    .df[[cast_col]] <- vec_cast(.df[[cast_col]], values_ptypes[[cast_col]])
  }

  # transform cols
  coerce_col <- intersect(values_to, names(values_transform))
  if (length(coerce_col) > 0) {
    f <- as_function(values_transform[[coerce_col]])
    .df[[coerce_col]] <- f(.df[[coerce_col]])
  }

  as_tidytable(.df)
}

