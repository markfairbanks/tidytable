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
summarize. <- function(.df, ..., .by = NULL, .sort = FALSE) {
  UseMethod("summarize.")
}

#' @export
summarize..data.frame <- function(.df, ..., .by = NULL, .sort = FALSE) {

  .df <- as_tidytable(.df)

  dots <- enquos(...)

  data_env <- env(quo_get_env(dots[[1]]), .df = .df)

  # Needed so n.() works
  dots <- map.(dots, replace_n_dot)

  .by <- select_vec_chr(.df, {{ .by }})

  assign <- map2.(syms(names(dots)), dots, ~ call2("<-", .x, .y))
  output <- call2("list", !!!syms(names(dots)))
  expr <- call2("{", !!!assign, output)

  if (.sort) {
    .df <- eval_quo(
      .df[, !!expr, keyby = !!.by],
      new_data_mask(data_env), env = caller_env()
    )

    setkey(.df, NULL)
  } else {
    .df <- eval_quo(
      .df[, !!expr, by = !!.by],
      new_data_mask(data_env), env = caller_env()
    )
  }

  df_name_repair(.df, .name_repair = "unique")
}

#' @export
#' @rdname summarize.
summarise. <- summarize.

replace_n_dot <- function(quosure) {
  quo_string <- quo_text(quosure)

  if (str_detect.(quo_string, "n.[(]")) {
    parse_expr(str_replace.(quo_string, "n.\\()", ".N"))
  } else {
    quosure
  }

}
