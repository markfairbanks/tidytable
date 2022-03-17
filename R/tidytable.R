#' Build a data.table/tidytable
#'
#' @description
#' Constructs a data.table, but one with nice printing features.
#'
#' @param ... A set of name-value pairs
#' @param .name_repair Treatment of duplicate names. See `?vctrs::vec_as_names` for options/details.
#'
#' @export
#'
#' @examples
#' tidytable(x = 1:3, y = c("a", "a", "b"))
tidytable <- function(..., .name_repair = c("check_unique", "unique", "universal", "minimal")) {
  dots <- dots_list(..., .named = TRUE)

  if (length(dots) == 0) {
    df <- new_tidytable()
  } else {
    dots <- map.(dots, ~ if (is_quosure(.x)) eval_tidy(.x) else .x)
    dots <- dots[!map_lgl.(dots, is.null)]

    dots <- vec_recycle_common(!!!dots)

    df <- df_name_repair(new_tidytable(dots), .name_repair = .name_repair)
  }

  df
}
