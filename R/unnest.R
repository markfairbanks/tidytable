#' Unnest a nested data.table
#'
#' @description
#' Unnest a nested data.table.
#'
#' @param .data A nested data.table
#' @param ... Columns to unnest. If empty, unnests all list columns. `tidyselect` compatible.
#'
#' @export
#' @md
#'
#' @examples
#' nested_df <- data.table::data.table(
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
unnest. <- function(.data, ...) {
  UseMethod("unnest.")
}

#' @export
unnest..data.frame <- function(.data, ...) {

  .data <- as_tidytable(.data)

  dots <- enexprs(...)

  data_names <- names(.data)

  list_flag <- map_lgl.(.data, is.list)

  if (length(dots) == 0) dots <- syms(data_names[list_flag])
  else dots <- dots_selector(.data, ...)

  unnest_data <- map.(dots, ~ unnest_col(.data, .x))

  unnest_nrow <- map_dbl.(unnest_data, nrow)

  if (!length(unique(unnest_nrow)) == 1)
    abort("unnested data contains different row counts")

  # Get cols to keep
  keep_cols <- data_names[!list_flag]

  # Get number of repeats for keep cols
  rep_vec <- map_dbl.(pull.(.data, !!dots[[1]]),
                      ~ fifelse(is.data.frame(.x), nrow(.x) %||% 1, length(.x)))
  keep_df <- .data[, ..keep_cols][rep(1:.N, rep_vec)]

  results_df <- bind_cols.(keep_df, unnest_data)

  results_df
}

#' @export
#' @rdname unnest.
dt_unnest_legacy <- unnest.

unnest_col <- function(.data, col = NULL) {
  # col <- enexpr(col)

  # Check if nested data is a data.frame, data.table, or vector
  nested_data <- pull.(.data, !!col)[[1]]
  is_datatable <- is.data.table(nested_data)
  is_dataframe <- is.data.frame(nested_data)

  if (is_dataframe) {
    if (!is_datatable) {
      .data <- shallow(.data)

      eval_expr(.data[, !!col := map.(!!col, as_tidytable)])
    }

    .data <- bind_rows.(pull.(.data, !!col))

  } else {
    # Unnests a vector
    .data <- eval_expr(
      .data[, list(.new_col = unlist(!!col, recursive = FALSE))]
    ) %>%
      rename.(!!col := .new_col)
  }
  .data
}

