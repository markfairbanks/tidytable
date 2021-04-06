#' Aggregate data using summary statistics
#'
#' @description
#' Aggregate data using summary statistics such as mean or median. Can be calculated by group.
#'
#' @param .df A data.frame or data.table
#' @param ... Aggregations to perform
#' @param .by Columns to group by.
#' * A single column can be passed with `.by = d`.
#' * Multiple columns can be passed with `.by = c(c, d)`
#' * `tidyselect` can be used:
#'   + Single predicate: `.by = where(is.character)`
#'   + Multiple predicates: `.by = c(where(is.character), where(is.factor))`
#'   + A combination of predicates and column names: `.by = c(where(is.character), b)`
#' @param .sort _experimental_: Should the resulting data.table be sorted by the grouping columns?
#'
#' @export
#' @md
#' @examples
#' test_df <- data.table(
#'   a = 1:3,
#'   b = 4:6,
#'   c = c("a","a","b"),
#'   d = c("a","a","b")
#' )
#'
#' test_df %>%
#'   summarize.(avg_a = mean(a),
#'              max_b = max(b),
#'              .by = c)
#'
#' test_df %>%
#'   summarize.(avg_a = mean(a),
#'              .by = c(c, d))
summarize. <- function(.df, ..., .by = NULL, .sort = FALSE) {
  UseMethod("summarize.")
}

#' @export
summarize..data.frame <- function(.df, ..., .by = NULL, .sort = FALSE) {
  .df <- as_tidytable(.df)

  dots <- enquos(...)

  mask <- build_data_mask(dots)

  dots <- prep_exprs(dots, .df, {{ .by }})

  .by <- select_vec_chr(.df, {{ .by }})

  j <- expr(list(!!!dots))

  dt_expr <- call2_j(.df, j, .by)

  .df <- eval_tidy(dt_expr, mask, caller_env())

  if (.sort) {
    .df <- arrange.(.df, !!!syms(.by))
  }

  df_name_repair(.df, .name_repair = "unique")
}

#' @export
#' @rdname summarize.
summarise. <- summarize.
