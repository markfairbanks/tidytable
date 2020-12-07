#' Expand a data.table to use all combinations of values
#'
#' @description
#' Generates all combinations of variables found in a dataset.
#'
#' `expand.()` is useful in conjunction with joins:
#' * use with `right_join.()` to convert implicit missing values to explicit missing values
#' * use with `anti_join.()` to find out which combinations are missing
#'
#' @param .df A data.frame or data.table
#' @param ... Columns to get combinations of
#' @param .name_repair Treatment of duplicate names. See `?vctrs::vec_as_names` for options/details
#'
#' @md
#' @export
#'
#' @examples
#' test_df <- tidytable(x = 1:2, y = 1:2)
#'
#' test_df %>%
#'   expand.(x, y)
expand. <- function(.df, ..., .name_repair = "check_unique") {
  UseMethod("expand.")
}

#' @export
expand..data.frame <- function(.df, ..., .name_repair = "check_unique") {

  .df <- as_tidytable(.df)

  dots <- enquos(...)

  # Remove NULL inputs
  dots <- dots[!map_lgl.(dots, quo_is_null)]

  if (length(dots) == 0) return(.df)

  data_vars <- unclass(.df)

  data_env <- env(quo_get_env(dots[[1]]), !!!data_vars)

  result_df <- eval_quo(
    data.table::CJ(!!!dots, sorted = TRUE, unique = TRUE),
    new_data_mask(data_env), env = caller_env()
  )

  setkey(result_df, NULL)

  result_df <- df_name_repair(result_df, .name_repair = .name_repair)

  as_tidytable(result_df)
}
