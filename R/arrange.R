#' Arrange/reorder rows
#'
#' @description Order rows in ascending or descending order.
#'
#' Note: `data.table` orders character columns slightly differently than `dplyr::arrange()` by
#' ordering in the "C-locale". See `?data.table::setorder` for more details.
#'
#' @param .df A data.frame or data.table
#' @param ... Variables to arrange by
#'
#' @export
#'
#' @examples
#' test_df <- data.table(
#'   a = c(1,2,3),
#'   b = c(4,5,6),
#'   c = c("a","a","b"))
#'
#' test_df %>%
#'   arrange.(c, -a)
arrange. <- function(.df, ...) {
  UseMethod("arrange.")
}

#' @export
arrange..data.frame <- function(.df, ...) {
  .df <- as_tidytable(.df)

  dots <- enquos(...)

  if (length(dots) == 0) return(.df)

  dots <- map.(dots, desc_as_minus)

  eval_quo(
    .df[order(!!!dots)]
  )
}

desc_as_minus <- function(quosure) {
  if (quo_is_call(quosure, "desc.")) {
    expr_str <- quo_text(node_cadr(quo_get_expr(quosure)))
    expr_str <- str_c.("-", expr_str)

    quosure <- new_quosure(
      parse_expr(expr_str),
      quo_get_env(quosure)
    )
  }
  quosure
}

#' @export
#' @rdname dt_verb
#' @inheritParams arrange.
dt_arrange <- function(.df, ...) {
  deprecate_stop("0.5.2", "tidytable::dt_arrange()", "arrange.()")

  arrange.(.df, ...)
}
