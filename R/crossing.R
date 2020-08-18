#' Create a data.table from all unique combinations of inputs
#'
#' @description
#' `crossing.()` is similar to `expand_grid.()` but de-duplicates and sorts its inputs.
#'
#' @param ... Variables to get unique combinations of
#' @param .name_repair Treatment of problematic names. See `?vctrs::vec_as_names` for options/details
#'
#' @md
#' @export
#'
#' @examples
#' x <- 1:2
#' y <- 1:2
#'
#' crossing.(x, y)
#'
#' crossing.(stuff = x, y)
crossing. <- function(..., .name_repair = "check_unique") {

  dots <- enquos(...)

  data_env <- env(quo_get_env(dots[[1]]))

  result_df <- eval_quo(
    data.table::CJ(!!!dots, sorted = TRUE, unique = TRUE),
    new_data_mask(data_env), env = caller_env()
  )

  setkey(result_df, NULL)

  old_names <- names(result_df)

  new_names <- vec_as_names(old_names, repair = .name_repair)

  setnames(result_df, old_names, new_names)

  as_tidytable(result_df)
}
