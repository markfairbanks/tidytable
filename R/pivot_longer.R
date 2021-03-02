#' Pivot data from wide to long
#'
#' @description
#' `pivot_longer.()` "lengthens" the data, increasing the number of rows and decreasing
#' the number of columns.
#'
#' @param .df A data.table or data.frame
#' @param cols Columns to pivot. `tidyselect` compatible.
#' @param names_to Name of the new "names" column. Must be a string.
#' @param values_to Name of the new "values" column. Must be a string.
#' @param names_prefix Remove matching text from the start of selected columns using regex.
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
#'   pivot_longer.(cols = c(x, y))
#'
#' test_df %>%
#'   pivot_longer.(cols = -z, names_to = "stuff", values_to = "things")
pivot_longer. <- function(.df,
                          cols = everything(),
                          names_to = "name",
                          values_to = "value",
                          names_prefix = NULL,
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
                                     names_prefix = NULL,
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

  measure_vars <- select_vec_chr(.df, {{ cols }})

  if (length(measure_vars) == 0) abort("At least one column must be supplied to cols")

  id_vars <- names[!names %in% measure_vars]

  multiple_names_to <- length(names_to) > 1
  uses_dot_value <- ".value" %in% names_to

  variable_name <- "variable"

  if (uses_dot_value) {
    if (!is.null(names_sep)) {
      .value <- str_separate(measure_vars, into = names_to, sep = names_sep)$.value
    } else if (!is.null(names_pattern)) {
      .value <- str_extract(measure_vars, into = names_to, names_pattern)$.value
    } else {
      abort("If you use '.value' in `names_to` you must also supply
            `names_sep' or `names_pattern")
    }

    v_fct <- factor(.value, levels = unique(.value))
    measure_vars <- split(measure_vars, v_fct)
    values_to <- names(measure_vars)
    names(measure_vars) <- NULL

    if (multiple_names_to) {
      variable_name <- names_to[!names_to == ".value"]
    }
  } else if (multiple_names_to) {
    if (is.null(names_sep) && is.null(names_pattern)) {
      abort("If you supply multiple names in `names_to` you must also
            supply `names_sep` or `names_pattern`")
    } else if (!is.null(names_sep) && !is.null(names_pattern)) {
      abort("only one of names_sep or names_pattern should be provided")
    }

    variable_name <- paste0(names_to, collapse = "___")
  } else {
    variable_name <- names_to
  }

  .df <- melt(
    data = .df,
    id.vars = id_vars,
    measure.vars = measure_vars,
    variable.name = variable_name,
    value.name = values_to,
    ...,
    # na.rm = values_drop_na,
    variable.factor = fast_pivot,
    value.factor = FALSE
  )

  if (!is.null(names_prefix)) {
    .df[[variable_name]] <- gsub(paste0("^", names_prefix), "", .df[[variable_name]])
  }

  if (multiple_names_to && !uses_dot_value) {
    if (!is.null(names_sep)) {
      .df <- separate.(.df, !!sym(variable_name), into = names_to, sep = names_sep)
    } else {
      .df <- extract.(.df, !!sym(variable_name), into = names_to, regex = names_pattern)
    }

    # Put new names before value column
    .df <- relocate.(.df, !!!syms(names_to), .before = !!sym(values_to))
  } else if (!multiple_names_to && uses_dot_value) {
    .df <- mutate.(.df, variable = NULL)
  }

  .df <- df_name_repair(.df, .name_repair = names_repair)

  ## names_ptype & names_transform
  # optionally, cast variables generated from columns
  cast_vars <- intersect(names_to, names(names_ptypes))
  if (length(cast_vars) > 0) {
    cast_calls <- vector("list", length(cast_vars))
    names(cast_calls) <- cast_vars
    for (i in seq_along(cast_vars)) {
      cast_calls[[i]] <- call2("vec_cast", sym(cast_vars[[i]]), names_ptypes[[i]])
    }
    .df <- mutate.(.df, !!!cast_calls)
  }

  # transform cols
  coerce_vars <- intersect(names_to, names(names_transform))
  if (length(coerce_vars) > 0) {
    coerce_calls <- vector("list", length(coerce_vars))
    names(coerce_calls) <- coerce_vars
    for (i in seq_along(coerce_vars)) {
      fn <- as_function(names_transform[[i]])
      coerce_calls[[i]] <- call2(fn, sym(coerce_vars[[i]]))
    }
    .df <- mutate.(.df, !!!coerce_calls)
  }

  ## values_ptype & values_transform
  # optionally, cast variables generated from columns
  cast_vars <- intersect(values_to, names(values_ptypes))
  if (length(cast_vars) > 0) {
    cast_calls <- vector("list", length(cast_vars))
    names(cast_calls) <- cast_vars
    for (i in seq_along(cast_vars)) {
      cast_calls[[i]] <- call2("vec_cast", sym(cast_vars[[i]]), values_ptypes[[i]])
    }
    .df <- mutate.(.df, !!!cast_calls)
  }

  # transform cols
  coerce_vars <- intersect(values_to, names(values_transform))
  if (length(coerce_vars) > 0) {
    coerce_calls <- vector("list", length(coerce_vars))
    names(coerce_calls) <- coerce_vars
    for (i in seq_along(coerce_vars)) {
      fn <- as_function(values_transform[[i]])
      coerce_calls[[i]] <- call2(fn, sym(coerce_vars[[i]]))
    }
    .df <- mutate.(.df, !!!coerce_calls)
  }

  # data.table::melt() drops NAs using "&" logic, not "|"
  # Example in tidytable #186 shows why this is necessary
  if (values_drop_na) {
    filter_calls <- vector("list", length(values_to))
    for (i in seq_along(filter_calls)) {
      filter_call <- call2("is.na", sym(values_to[[i]]))
      filter_call <- call2("!", filter_call)
      filter_calls[[i]] <- filter_call
    }

    filter_expr <- call_reduce(filter_calls, "|")

    .df <- filter.(.df, !!filter_expr)
  }

  as_tidytable(.df)
}

call_reduce <- function(x, fun) {
  Reduce(function(x, y) call2(fun, x, y), x)
}

str_extract <- function(x, into, regex, convert = FALSE) {
  out <- str_extract_groups(x, pattern = regex, convert = convert)
  out <- as_tidytable(out)
  names(out) <- into
  out
}

str_separate <- function(x, into, sep, convert = FALSE) {
  vec_assert(into, character())

  out <- data.table::tstrsplit(x, sep, fixed = TRUE, names = TRUE, type.convert = convert)
  out <- as_tidytable(out)
  names(out) <- as_utf8_character(into)

  out
}

