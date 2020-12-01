#' Create a data.table from all combinations of inputs
#'
#' @description
#' Create a data.table from all combinations of inputs
#'
#' @param ... Variables to get combinations of
#' @param .name_repair Treatment of problematic names. See `?vctrs::vec_as_names` for options/details
#'
#' @md
#' @export
#'
#' @examples
#' x <- 1:2
#' y <- 1:2
#'
#' expand_grid.(x, y)
#'
#' expand_grid.(stuff = x, y)
expand_grid. <- function(..., .name_repair = "check_unique") {

  result_df <- CJ(..., unique = FALSE, sorted = FALSE)

  setkey(result_df, NULL)

  result_df <- df_name_repair(result_df, .name_repair = .name_repair)

  as_tidytable(result_df)
}
