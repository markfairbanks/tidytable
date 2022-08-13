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
#'
#' @examples
#' df <- data.table(
#'   x = 1:3,
#'   y = 4:6,
#'   z = c("a", "b", "c")
#' )
#'
#' df %>%
#'   pivot_longer.(cols = c(x, y))
#'
#' df %>%
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
pivot_longer..tidytable <- function(.df,
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
  names <- names(.df)

  measure_vars <- tidyselect_names(.df, {{ cols }})

  if (length(measure_vars) == 0) abort("At least one column must be supplied to cols")

  id_vars <- names[!names %in% measure_vars]

  multiple_names_to <- length(names_to) > 1
  uses_dot_value <- ".value" %in% names_to
  na_in_names_to <- is.na(names_to)

  variable_name <- "variable"

  if (uses_dot_value) {
    if (multiple_names_to && any(na_in_names_to)) {
      names_to[na_in_names_to] <- ".id"
    } else if (!multiple_names_to) {
      abort("The use of `names_to = '.value'` is not yet supported")
    }

    if (!is.null(names_sep)) {
      names_to_setup <- str_separate(measure_vars, into = names_to, sep = names_sep)

      names_glue <- paste0("{", names_to, "}", collapse = names_sep)
    } else if (!is.null(names_pattern)) {
      names_to_setup <- str_extract(measure_vars, into = names_to, names_pattern)

      names_glue <- paste0("{", names_to, "}", collapse = "___")
      new_names <- glue_data(names_to_setup, names_glue)

      .df <- df_set_names(.df, new_names, measure_vars)

      measure_vars <- new_names
    } else {
      abort("If you use '.value' in `names_to` you must also supply
            `names_sep' or `names_pattern")
    }

    .value <- unique(names_to_setup$.value)

    if (multiple_names_to) {
      variable_name <- names_to[!names_to == ".value"]
      .value_ids <- f_sort(unique(names_to_setup[[variable_name]]))
      names_to_setup <- expand_grid.(.value = .value, !!variable_name := .value_ids)
    }

    glued <- glue_data(names_to_setup, names_glue)

    na_cols <- setdiff(glued, measure_vars)

    if (length(na_cols) > 0) {
      .df <- dt_j(.df, (na_cols) := NA)
    }

    .value <- names_to_setup$.value
    v_fct <- factor(.value, levels = unique(.value))

    measure_vars <- split(glued, v_fct)
    values_to <- names(measure_vars)
    names(measure_vars) <- NULL

    if (multiple_names_to) {
      .value_ids <- unique(names_to_setup[[variable_name]])
      .value_ids <- vec_rep_each(.value_ids, nrow(.df))
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

  if (values_drop_na && !multiple_names_to) {
    na_rm <- TRUE
  } else {
    na_rm <- FALSE
  }

  out <- suppressWarnings(melt(
    data = .df,
    id.vars = id_vars,
    measure.vars = measure_vars,
    variable.name = variable_name,
    value.name = values_to,
    ...,
    na.rm = na_rm,
    variable.factor = fast_pivot,
    value.factor = TRUE
  ))

  if (!is.null(names_prefix)) {
    out <- mutate.(out, !!variable_name := gsub(paste0("^", .env$names_prefix), "", !!sym(variable_name)))
  }

  if (multiple_names_to && uses_dot_value) {
    if (any(na_in_names_to)) {
      .value_ids <- NULL
    }

    out <- mutate.(out, !!variable_name := .env$.value_ids)
  } else if (multiple_names_to && !uses_dot_value) {
    if (!is.null(names_sep)) {
      out <- separate.(out, !!sym(variable_name), into = names_to, sep = names_sep)
    } else {
      out <- extract.(out, !!sym(variable_name), into = names_to, regex = names_pattern)
    }

    # Put new names before value column
    out <- relocate.(out, !!!syms(names_to), .before = !!sym(values_to))
  } else if (!multiple_names_to && uses_dot_value) {
    out <- dt_j(out, !!variable_name := NULL)
  }

  out <- df_name_repair(out, .name_repair = names_repair)

  # names_ptype & names_transform
  out <- change_types(out, names_to, names_ptypes, "ptypes")
  out <- change_types(out, names_to, names_transform, "transform")

  # values_ptype & values_transform
  out <- change_types(out, values_to, values_ptypes, "ptypes")
  out <- change_types(out, values_to, values_transform, "transform")

  # data.table::melt() drops NAs using "&" logic, not "|"
  # See issue #186
  if (values_drop_na && multiple_names_to) {
    out <- filter.(out, if_any.(any_of(values_to), ~ !is.na(.x)))
  }

  as_tidytable(out)
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
  pivot_longer.(
    .df, cols = {{ cols }}, names_to = names_to,
    values_to = values_to, names_prefix = names_prefix,
    names_sep = names_sep, names_pattern = names_pattern,
    names_ptypes = names_ptypes, names_transform = names_transform,
    names_repair = names_repair, values_drop_na = values_drop_na,
    values_ptypes = values_ptypes, values_transform = values_transform,
    fast_pivot = fast_pivot
  )
}

str_extract <- function(x, into, regex, convert = FALSE) {
  out <- str_extract_groups(x, pattern = regex, convert = convert)
  names(out) <- into
  out <- new_tidytable(out)
  out
}

str_separate <- function(x, into, sep, convert = FALSE) {
  out <- data.table::tstrsplit(x, sep, fixed = TRUE, names = TRUE, type.convert = convert)
  names(out) <- as_utf8_character(into)
  out <- new_tidytable(out)
  out
}

