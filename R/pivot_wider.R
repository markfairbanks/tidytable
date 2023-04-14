#' Pivot data from long to wide
#'
#' @description "Widens" data, increasing the number of columns and
#'   decreasing the number of rows.
#'
#' @param .df A data.frame or data.table
#' @param id_cols A set of columns that uniquely identifies each observation.
#'   Defaults to all columns in the data table except for the columns specified in `names_from` and `values_from`.
#'   Typically used when you have additional variables that is directly related.
#'   `tidyselect` compatible.
#' @param names_from A pair of arguments describing which column (or columns) to get the name of the output column `name_from`,
#'   and which column (or columns) to get the cell values from `values_from`).
#'   `tidyselect` compatible.
#' @param values_from A pair of arguments describing which column (or columns) to get the name of the output column `name_from`,
#'   and which column (or columns) to get the cell values from `values_from`.
#'   `tidyselect` compatible.
#' @param names_sep the separator between the names of the columns
#' @param names_prefix prefix to add to the names of the new columns
#' @param names_glue Instead of using `names_sep` and `names_prefix`, you can supply a
#'   glue specification that uses the `names_from` columns (and special `.value`) to create custom column names
#' @param names_sort Should the resulting new columns be sorted.
#' @param names_repair Treatment of duplicate names. See `?vctrs::vec_as_names` for options/details.
#' @param values_fn Should the data be aggregated before casting? If the formula doesn't identify a single observation for each cell, then aggregation defaults to length with a message.
#' @param values_fill If values are missing, what value should be filled in
#'
#' @examples
#' df <- tidytable(
#'   id = 1,
#'   names = c("a", "b", "c"),
#'   vals = 1:3
#' )
#'
#' df %>%
#'   pivot_wider(names_from = names, values_from = vals)
#'
#' df %>%
#'   pivot_wider(
#'     names_from = names, values_from = vals, names_prefix = "new_"
#'   )
#' @export
pivot_wider <- function(.df,
                        names_from = name,
                        values_from = value,
                        id_cols = NULL,
                        names_sep = "_",
                        names_prefix = "",
                        names_glue = NULL,
                        names_sort = FALSE,
                        names_repair = "unique",
                        values_fill = NULL,
                        values_fn = NULL) {
  .df <- .df_as_tidytable(.df)

  names_from <- tidyselect_names(.df, {{ names_from }})
  values_from <- tidyselect_names(.df, {{ values_from }})

  id_cols <- enquo(id_cols)
  if (quo_is_null(id_cols)) {
    id_cols <- setdiff(names(.df), c(names_from, values_from))
  } else {
    id_cols <- tidyselect_names(.df, !!id_cols)
  }

  values_fn <- quo_squash(enquo(values_fn))

  uses_dot_value <- !is.null(names_glue) && str_detect(names_glue, "{.value}", fixed = TRUE)

  # Prepare output column names
  if (names_prefix != "" && is.null(names_glue)) {
    first_name <- sym(names_from[[1]])

    .df <- mutate(.df, !!first_name := paste0(.env$names_prefix, !!first_name))
  } else if (uses_dot_value) {
    glue_df <- distinct(.df, all_of(names_from))
    values_from_reps <- nrow(glue_df)
    glue_df <- vec_rep(glue_df, length(values_from))
    glue_df <- mutate(glue_df,
                      .value = vec_rep_each(.env$values_from, .env$values_from_reps),
                      .before = 1)

    glue_vars <- glue_data(glue_df, names_glue)
    # mimic column names assigned by data.table::dcast()
    if (length(values_from) <= 1) {
      glue_df <- dt_j(glue_df, .value := NULL)
    }
    names(glue_vars) <- exec(paste, !!!glue_df, sep = names_sep)
  } else if (!is.null(names_glue)) {
    .df <- mutate(.df,
                  .names_from = glue(.env$names_glue),
                  .before = all_of(names_from[1]))
    .df <- dt_j(.df, (names_from) := NULL)

    names_from <- ".names_from"
  }

  if (is_false(names_sort)) {
    .df <- mutate(.df, across(all_of(names_from), ~ safe_as_factor(.x)))
  }

  no_id <- length(id_cols) == 0

  if (no_id) {
    lhs <- "..."
  } else {
    lhs <- str_flatten(glue("`{id_cols}`"), " + ")
  }

  rhs <- str_flatten(glue("`{names_from}`"), " + ")

  formula <- glue("{lhs} ~ {rhs}")

  dcast_call <- call2(
    "dcast",
    quo(.df),
    formula = formula,
    value.var = values_from,
    fun.aggregate = expr(!!values_fn),
    sep = names_sep,
    fill = values_fill,
    .ns = "data.table"
  )

  out <- eval_tidy(dcast_call)

  out <- remove_key(out)

  if (no_id) {
    out <- dt_j(out, . := NULL)
  }

  if (uses_dot_value) {
    new_vars <- setdiff(names(out), id_cols)

    out <- df_set_names(out, glue_vars[new_vars], new_vars)
  }

  out <- df_name_repair(out, names_repair)

  as_tidytable(out)
}

safe_as_factor <- function(x) {
  if (vec_ptype_compatible(x, factor())) {
    vec_cast(x, factor())
  } else if (is.numeric(x)) {
    factor(x, vec_unique(x))
  } else {
    x
  }
}

#' @export
#' @keywords internal
#' @inherit pivot_wider
pivot_wider. <- function(.df,
                         names_from = name,
                         values_from = value,
                         id_cols = NULL,
                         names_sep = "_",
                         names_prefix = "",
                         names_glue = NULL,
                         names_sort = FALSE,
                         names_repair = "unique",
                         values_fill = NULL,
                         values_fn = NULL) {
  deprecate_dot_fun()
  pivot_wider(
    .df, names_from = {{ names_from }}, values_from = {{ values_from }},
    id_cols = {{ id_cols }}, names_sep = names_sep,
    names_prefix = names_prefix, names_glue = names_glue,
    names_sort = names_sort, names_repair = names_repair,
    values_fill = values_fill, values_fn = {{ values_fn }}
  )
}

globalVariables(c(".", ".names_from", "name", "value", ".value"))
