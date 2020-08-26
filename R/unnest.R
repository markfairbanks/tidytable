#' Unnest a nested data.table
#'
#' @description
#' Unnest a nested data.table.
#'
#' @param .df A nested data.table
#' @param ... Columns to unnest. If empty, unnests all list columns. `tidyselect` compatible.
#' @param .keep_all Should list columns that were not unnested be kept
#'
#' @export
#' @md
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
#'   unnest.(data, pulled_vec)
unnest. <- function(.df, ..., .keep_all = FALSE) {
  UseMethod("unnest.")
}

#' @export
unnest..data.frame <- function(.df, ..., .keep_all = FALSE) {

  .df <- as_tidytable(.df)

  vec_assert(.keep_all, logical(), 1)

  dots <- enquos(...)

  data_names <- names(.df)

  list_flag <- map_lgl.(.df, is.list)

  if (length(dots) == 0) dots <- syms(data_names[list_flag])
  else dots <- select_dots_sym(.df, ...)

  keep_cols <- data_names[!list_flag]

  if (.keep_all) {
    list_cols <- data_names[list_flag]

    keep_cols <- c(keep_cols, list_cols[list_cols %notin% as.character(dots)])
  }

  unnest_data <- map.(dots, ~ unnest_col(.df, .x))

  unnest_nrow <- list_sizes(unnest_data)

  if (!length(vec_unique(unnest_nrow)) == 1)
    abort("unnested data contains different row counts")

  # Get number of repeats for keep cols
  rep_vec <- list_sizes(pull.(.df, !!dots[[1]]))

  keep_df <- .df[, ..keep_cols][vec_rep_each(1:.N, rep_vec)]

  results_df <- bind_cols.(keep_df, unnest_data)

  results_df
}

#' @export
#' @rdname dt_verb
#' @inheritParams unnest.
dt_unnest_legacy <- function(.df, ..., .keep_all = FALSE) {
  deprecate_warn("0.5.2", "tidytable::dt_unnest_legacy()", "unnest.()")

  unnest.(.df, ..., .keep_all = .keep_all)
}

unnest_col <- function(.df, col = NULL) {

  # Check if nested data is a vector
  nested_data <- pull.(.df, !!col)[[1]]
  is_vec <- any(vec_in(class(nested_data), c("character", "factor", "numeric", "integer")))

  if (is_vec) {

    result_df <- summarize.(.df, !!col := unlist(!!col, recursive = FALSE))

  } else {

    # bind_rows.() auto-converts lists of data.frames/tibbles/matrices to data.tables
    result_df <- bind_rows.(pull.(.df, !!col))
  }

  result_df
}

