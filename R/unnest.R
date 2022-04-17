#' Unnest list-columns
#'
#' @description
#' Unnest list-columns.
#'
#' @param .df A data.table
#' @param ... Columns to unnest. If empty, unnests all list columns. `tidyselect` compatible.
#' @param keep_empty Return `NA` for any `NULL` elements of the list column
#' @param .drop Should list columns that were not unnested be dropped
#' @param names_sep If NULL, the default, the inner column names will become the new outer column names.
#'
#'   If a string, the name of the outer column will be appended to the beginning of the inner column names,
#'   with `names_sep` used as a separator.
#'
#' @param names_repair Treatment of duplicate names. See `?vctrs::vec_as_names` for options/details.
#'
#' @export
#'
#' @examples
#' nested_df <-
#'   data.table(
#'     a = 1:10,
#'     b = 11:20,
#'     c = c(rep("a", 6), rep("b", 4)),
#'     d = c(rep("a", 4), rep("b", 6))
#'   ) %>%
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
                    keep_empty = FALSE,
                    .drop = TRUE,
                    names_sep = NULL,
                    names_repair = "unique") {
  UseMethod("unnest.")
}

#' @export
unnest..tidytable <- function(.df,
                              ...,
                              keep_empty = FALSE,
                              .drop = TRUE,
                              names_sep = NULL,
                              names_repair = "unique") {
  vec_assert(.drop, logical(), 1)

  dots <- enquos(...)

  data_names <- names(.df)

  list_bool <- map_lgl.(.df, is.list)

  if (length(dots) == 0) {
    dots <- syms(data_names[list_bool])
  } else {
    dots <- tidyselect_syms(.df, ...)
  }

  if (.drop) {
    keep_cols <- data_names[!list_bool]
  } else {
    keep_cols <- data_names[data_names %notin% as.character(dots)]
  }

  if (keep_empty) {
    dots_chr <- as.character(dots)
    .df <- mutate.(.df, across.(all_of(dots_chr), keep_empty_prep))
  }

  unnest_data <- map.(dots, ~ unnest_col(.df, .x, names_sep))

  unnest_nrow <- list_sizes(unnest_data)

  if (!length(vec_unique(unnest_nrow)) == 1) {
    abort("unnested data contains different row counts")
  }

  # Get number of repeats for keep cols
  rep_vec <- list_sizes(pull.(.df, !!dots[[1]]))

  if (length(keep_cols) > 0) {
    keep_df <- .df[, ..keep_cols]

    keep_df <- keep_df[vec_rep_each(1:.N, rep_vec)]

    result_df <- bind_cols.(keep_df, unnest_data, .name_repair = names_repair)
  } else {
    result_df <- bind_cols.(unnest_data, .name_repair = names_repair)
  }

  result_df
}

#' @export
unnest..data.frame <- function(.df,
                               ...,
                               keep_empty = FALSE,
                               .drop = TRUE,
                               names_sep = NULL,
                               names_repair = "unique") {
  .df <- as_tidytable(.df)
  unnest.(
    .df, ..., keep_empty = keep_empty, .drop = .drop,
    names_sep = names_sep, names_repair = names_repair
  )
}

unnest_col <- function(.df, col = NULL, names_sep = NULL) {
  .l <- pull.(.df, !!col)

  .l <- list_drop_empty(.l)

  .check_data <- .l[[1]]
  is_vec <- is.atomic(.check_data) && !is.matrix(.check_data)

  if (is_vec) {
    # Use do.call so lists of dates are not unclassed by unlist
    result_df <- tidytable(!!col := do.call("c", .l))
  } else {
    result_df <- bind_rows.(.l)
  }

  if (!is.null(names_sep)) {
    names(result_df) <- paste(as_name(col), names(result_df), sep = names_sep)
  }

  result_df
}

keep_empty_prep <- function(.l) {
  null_bool <- map_lgl.(.l, is.null)

  if (!any(null_bool)) return(.l)

  .check_data <- .l[!null_bool][[1]]
  is_vec <- is.atomic(.check_data) && !is.matrix(.check_data)

  if (is_vec) {
    .replace <- NA
  } else {
    null_df <- vec_slice(.check_data, 1)
    null_df <- vec_assign(null_df, 1, NA)

    .replace <- list(null_df)
  }

  replace_na.(.l, .replace)
}

globalVariables("..keep_cols")
