#' Unnest list-columns
#'
#' @description
#' Unnest list-columns.
#'
#' @param .df A data.table
#' @param ... Columns to unnest If empty, unnests all list columns. `tidyselect` compatible.
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
#' df1 <- tidytable(x = 1:3, y = 1:3)
#' df2 <- tidytable(x = 1:2, y = 1:2)
#' nested_df <-
#'   data.table(
#'     a = c("a", "b"),
#'     frame_list = list(df1, df2),
#'     vec_list = list(4:6, 7:8)
#'   )
#'
#' nested_df %>%
#'   unnest(frame_list)
#'
#' nested_df %>%
#'   unnest(frame_list, names_sep = "_")
#'
#' nested_df %>%
#'   unnest(frame_list, vec_list)
unnest <- function(.df,
                   ...,
                   keep_empty = FALSE,
                   .drop = TRUE,
                   names_sep = NULL,
                   names_repair = "unique") {
  unnest.(
    .df, ..., keep_empty = keep_empty, .drop = .drop,
    names_sep = names_sep, names_repair = names_repair
  )
}


#' @export
#' @keywords internal
#' @inherit unnest
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

  df_names <- names(.df)

  .is_list <- map_lgl(.df, is.list)

  if (length(dots) == 0) {
    dots <- syms(df_names[.is_list])
  } else {
    dots <- tidyselect_syms(.df, ...)
  }

  if (keep_empty) {
    dots_chr <- as.character(dots)
    .df <- mutate(.df, across(all_of(dots_chr), keep_empty_prep))
  }

  unnested <- map(dots, ~ unnest_col(.df, .x, names_sep))

  if (!list_all_size(unnested, vec_size(unnested[[1]]))) {
    abort("unnested data contains different row counts")
  }

  if (.drop) {
    cols_keep <- df_names[!.is_list]
  } else {
    cols_keep <- setdiff(df_names, as.character(dots))
  }

  if (length(cols_keep) > 0) {
    # Get number of repeats for keep cols
    reps <- list_sizes(pull(.df, !!dots[[1]]))

    keep_df <- select(.df, any_of(cols_keep))

    keep_df <- vec_rep_each(keep_df, reps)

    out_df <- bind_cols(keep_df, unnested, .name_repair = names_repair)
  } else {
    out_df <- bind_cols(unnested, .name_repair = names_repair)
  }

  out_df
}

#' @export
unnest..data.frame <- function(.df,
                               ...,
                               keep_empty = FALSE,
                               .drop = TRUE,
                               names_sep = NULL,
                               names_repair = "unique") {
  .df <- as_tidytable(.df)
  unnest(
    .df, ..., keep_empty = keep_empty, .drop = .drop,
    names_sep = names_sep, names_repair = names_repair
  )
}

unnest_col <- function(.df, col = NULL, names_sep = NULL) {
  .l <- pull(.df, !!col)

  .l <- list_drop_empty(.l)

  if (length(.l) == 0) {
    .l <- list(logical())
  }

  first <- .l[[1]]
  is_vec <- is_simple_vector(first)

  if (is_vec) {
    # Use do.call so lists of dates are not unclassed by unlist
    out_df <- tidytable(!!col := do.call("c", .l))
  } else {
    out_df <- bind_rows(.l)
  }

  if (!is.null(names_sep)) {
    names(out_df) <- paste(as_name(col), names(out_df), sep = names_sep)
  }

  out_df
}

keep_empty_prep <- function(.l) {
  .is_null <- vec_detect_missing(.l)

  if (!any(.is_null)) {
    return(.l)
  }

  first <- .l[!.is_null][[1]]
  is_vec <- is_simple_vector(first)

  if (is_vec) {
    .replace <- NA
  } else {
    null_df <- vec_init(first, 1)

    .replace <- list(null_df)
  }

  replace_na(.l, .replace)
}
