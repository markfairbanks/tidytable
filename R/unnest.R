#' Unnest a nested data.table
#'
#' @description
#' Unnest a nested data.table.
#'
#' @param .df A nested data.table
#' @param ... Columns to unnest. If empty, unnests all list columns. `tidyselect` compatible.
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
unnest. <- function(.df, ...) {
  UseMethod("unnest.")
}

#' @export
unnest..data.frame <- function(.df, ...) {

  .df <- as_tidytable(.df)

  dots <- enquos(...)

  data_names <- names(.df)

  list_flag <- map_lgl.(.df, is.list)

  if (length(dots) == 0) dots <- syms(data_names[list_flag])
  else dots <- select_dots_sym(.df, ...)

  unnest_data <- map.(dots, ~ unnest_col(.df, .x))

  unnest_nrow <- map_dbl.(unnest_data, nrow)

  if (!length(vec_unique(unnest_nrow)) == 1)
    abort("unnested data contains different row counts")

  # Get cols to keep (all non-list cols)
  keep_cols <- data_names[!list_flag]

  # Get number of repeats for keep cols
  rep_vec <- map_dbl.(pull.(.df, !!dots[[1]]), vec_size)

  keep_df <- .df[, ..keep_cols][rep(1:.N, rep_vec)]

  results_df <- bind_cols.(keep_df, unnest_data)

  results_df
}

#' @export
#' @rdname unnest.
dt_unnest_legacy <- unnest.

unnest_col <- function(.df, col = NULL) {

  # Check if nested data is a data.frame, data.table, or vector
  nested_data <- pull.(.df, !!col)[[1]]
  is_datatable <- is.data.table(nested_data)
  is_dataframe <- is.data.frame(nested_data)

  if (is_dataframe) {

    if (!is_datatable) .df <- mutate.(.df, !!col := map.(!!col, as_tidytable))

    result_df <- bind_rows.(pull.(.df, !!col))

  } else {
    # Unnests a vector
    result_df <- summarize.(.df, !!col := unlist(!!col, recursive = FALSE))
  }
  result_df
}

