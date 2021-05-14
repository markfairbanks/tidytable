#' Build a data.table/tidytable
#'
#' @description
#' `tidytable()` constructs a data.table, but one with nice printing features.
#'
#' @param ... Arguments passed to `data.table()`
#' @param .name_repair Treatment of duplicate names. See `?vctrs::vec_as_names` for options/details.
#'
#' @export
#'
#' @examples
#' tidytable(x = 1:3, y = c("a", "a", "b"))
tidytable <- function(..., .name_repair = c("check_unique", "unique", "universal", "minimal")) {
  dots <- enquos(...)

  dt_env <- build_dt_env(dots)

  dt_expr <- call2_dt("data.table", !!!dots)

  .df <- eval_tidy(dt_expr, env = dt_env)

  .df <- df_name_repair(.df, .name_repair = .name_repair)

  as_tidytable(.df)
}
