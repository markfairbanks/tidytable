#' Aggregate data using summary statistics
#'
#' @description
#' Aggregate data using summary statistics such as mean or median. Can be calculated by group.
#'
#' @param .df A data.frame or data.table
#' @param ... Aggregations to perform
#' @param .by Columns to group by.
#' * A single column can be passed with `by = d`.
#' * Multiple columns can be passed with `by = c(c, d)`
#' * `tidyselect` can be used:
#'   + Single predicate: `by = where(is.character)`
#'   + Multiple predicates: `by = c(where(is.character), where(is.factor))`
#'   + A combination of predicates and column names: `by = c(where(is.character), b)`
#' @param by This argument has been renamed to .by and is deprecated
#'
#' @export
#' @md
#' @examples
#' test_df <- data.table(
#'   a = c(1,2,3),
#'   b = c(4,5,6),
#'   c = c("a","a","b"),
#'   d = c("a","a","b"))
#'
#' test_df %>%
#'   summarize.(avg_a = mean(a),
#'              max_b = max(b),
#'              .by = c)
#'
#' test_df %>%
#'   summarize.(avg_a = mean(a),
#'              .by = c(c, d))
summarize. <- function(.df, ..., .by = NULL, by = NULL) {
  UseMethod("summarize.")
}

#' @export
summarize..data.frame <- function(.df, ..., .by = NULL, by = NULL) {

  .df <- as_tidytable(.df)

  dots <- enquos(...)

  data_env <- env(quo_get_env(dots[[1]]), .df = .df)

  # Needed so n.() works
  dots <- map.(dots, wrap_n_dot)

  .by <- check_dot_by(enquo(.by), enquo(by), "summarize.")
  .by <- select_vec_chr(.df, !!.by)

  eval_quo(
    .df[, list(!!!dots), by = !!.by],
    new_data_mask(data_env), env = caller_env()
  )

}

#' @export
#' @rdname summarize.
summarise. <- summarize.

#' @export
#' @rdname summarize.
dt_summarise <- function(.df, ..., .by = NULL, by = NULL) {
  deprecate_soft("0.5.2", "tidytable::dt_summarise()", "summarise.()")

  .by <- check_dot_by(enquo(.by), enquo(by))

  summarize.(.df, ..., .by = !!.by )
}

#' @export
#' @rdname summarize.
dt_summarize <- function(.df, ..., .by = NULL, by = NULL) {
  deprecate_soft("0.5.2", "tidytable::dt_summarize()", "summarize.()")

  .by <- check_dot_by(enquo(.by), enquo(by))

  summarize.(.df, ..., .by = {{ .by }})
}

wrap_n_dot <- function(quosure) {
  quo_string <- quo_text(quosure)

  if (str_detect.(quo_string, "n.[(]")) {
    parse_expr(str_c.("(", quo_string, ")"))
  } else {
    quo_squash(quosure)
  }

}
