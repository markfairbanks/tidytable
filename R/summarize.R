#' Aggregate data using summary statistics
#'
#' @description
#' Aggregate data using summary statistics such as mean or median. Can be calculated by group.
#'
#' @param .df A data.frame or data.table
#' @param ... Aggregations to perform
#' @param by Columns to group by.
#' * A single column can be passed with `by = d`.
#' * Multiple columns can be passed with `by = c(c, d)`
#' * `tidyselect` can be used:
#'   + Single predicate: `by = where(is.character)`
#'   + Multiple predicates: `by = c(where(is.character), where(is.factor))`
#'   + A combination of predicates and column names: `by = c(where(is.character), b)`
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
#'              by = c)
#'
#' test_df %>%
#'   summarize.(avg_a = mean(a),
#'              by = c(c, d))
summarize. <- function(.df, ..., by = NULL) {
  UseMethod("summarize.")
}

#' @export
summarize..data.frame <- function(.df, ..., by = NULL) {

  .df <- as_tidytable(.df)

  dots <- enquos(...)
  by <- select_vec_by(.df, {{ by }})

  eval_quo(
    .df[, eval_quo(
      {.N = .env$.N; .SD = .env$.SD; .I = .env$.I; .GRP = .env$.GRP;
      list(!!!dots)}, .SD), by = !!by],
    .df)

}

#' @export
#' @rdname summarize.
summarise. <- summarize.

#' @export
#' @rdname summarize.
dt_summarise <- summarize.

#' @export
#' @rdname summarize.
dt_summarize <- summarize.
