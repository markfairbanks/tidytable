#' Create a data.table from all combinations of inputs
#'
#' @description
#' Create a data.table from all combinations of inputs
#'
#' @param ... Variables to get combinations of
#' @param .name_repair Treatment of problematic names. See `?vctrs::vec_as_names` for options/details
#'
#' @export
#'
#' @examples
#' x <- 1:2
#' y <- 1:2
#'
#' expand_grid(x, y)
#'
#' expand_grid(stuff = x, y)
expand_grid <- function(..., .name_repair = "check_unique") {
  dots <- dots_list(..., .named = TRUE)

  is_data_frame <- map_lgl(dots, is.data.frame)

  out <- vec_expand_grid(!!!dots, .name_repair = "minimal")

  # df_list requires data frames inputs to be unnamed to unpack
  out <- as.list(out)
  names(out)[is_data_frame] <- ""
  out <- df_list(!!!out, .name_repair = .name_repair)

  new_tidytable(out)
}

#' @export
#' @keywords internal
#' @inherit expand_grid
expand_grid. <- expand_grid
