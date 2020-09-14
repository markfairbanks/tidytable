#' Unnest a nested data.table
#'
#' @description
#' Unnest a nested data.table.
#'
#' @param .df A nested data.table
#' @param ... Columns to unnest. If empty, unnests all list columns. `tidyselect` compatible.
#' @param .drop Should list columns that were not unnested be dropped
#' @param names_sep If NULL, the default, the inner column names will become the new outer column names.
#'
#' If a string, the name of the outer column will be appended to the beginning of the inner column names,
#' with `names_sep` used as a separator.
#'
#' @param names_repair Treatment of duplicate names. See `?vctrs::vec_as_names` for options/details.
#' @param .keep_all Deprecated. Please use `.drop = FALSE` to keep unused list columns.
#'
#' @export
#'
#' @examples
#' nested_df <- data.table(
#'   a = 1:10,
#'   b = 11:20,
#'   c = c(rep("a", 6), rep("b", 4)),
#'   d = c(rep("a", 4), rep("b", 6))) %>%
#'   nest_by.(c, d) %>%
#'   mutate.(pulled_vec = map.(data, ~ pull.(.x, a)))
#'
#' nested_df %>%
#'   unnest.(data)
#'
#' nested_df %>%
#'   unnest.(data, names_sep = "_")
#'
#' nested_df %>%
#'   unnest.(data, pulled_vec)
unnest. <- function(.df,
                    ...,
                    .drop = TRUE,
                    names_sep = NULL,
                    names_repair = "unique",
                    .keep_all = deprecated()) {
  UseMethod("unnest.")
}

#' @export
unnest..data.frame <- function(.df,
                               ...,
                               .drop = TRUE,
                               names_sep = NULL,
                               names_repair = "unique",
                               .keep_all = deprecated()) {

  .df <- as_tidytable(.df)

  vec_assert(.drop, logical(), 1)

  dots <- enquos(...)

  data_names <- names(.df)

  list_flag <- map_lgl.(.df, is.list)

  if (length(dots) == 0) dots <- syms(data_names[list_flag])
  else dots <- select_dots_sym(.df, ...)

  keep_cols <- data_names[!list_flag]

  if (!is_missing(.keep_all)) {
    .drop <- !.keep_all

    deprecate_warn("0.5.6", "tidytable::unnest.(.keep_all = )", "unnest.(.drop = )")
  }

  if (!.drop) {
    list_cols <- data_names[list_flag]

    keep_cols <- c(keep_cols, list_cols[list_cols %notin% as.character(dots)])
  }

  unnest_data <- map.(dots, ~ unnest_col(.df, .x, names_sep))

  unnest_nrow <- list_sizes(unnest_data)

  if (!length(vec_unique(unnest_nrow)) == 1)
    abort("unnested data contains different row counts")

  # Get number of repeats for keep cols
  rep_vec <- list_sizes(pull.(.df, !!dots[[1]]))

  keep_df <- .df[, ..keep_cols][vec_rep_each(1:.N, rep_vec)]

  results_df <- bind_cols.(keep_df, unnest_data, .name_repair = names_repair)

  results_df
}

#' @export
#' @rdname dt_verb
#' @inheritParams unnest.
dt_unnest_legacy <- function(.df, ..., .keep_all = FALSE) {
  deprecate_stop("0.5.2", "tidytable::dt_unnest_legacy()", "unnest.()")

  unnest.(.df, ..., .keep_all = .keep_all)
}

unnest_col <- function(.df, col = NULL, names_sep = NULL) {

  # Check if nested data is a vector
  nested_data <- pull.(.df, !!col)[[1]]
  is_vec <- any(vec_in(class(nested_data), c("character", "factor", "numeric", "integer")))

  if (is_vec) {

    result_df <- summarize.(.df, !!col := unlist(!!col, recursive = FALSE))

  } else {

    # bind_rows.() auto-converts lists of data.frames/tibbles/matrices to data.tables
    result_df <- bind_rows.(pull.(.df, !!col))
  }

  if (!is.null(names_sep))
    names(result_df) <- paste(quo_text(col), names(result_df), sep = names_sep)

  result_df
}

