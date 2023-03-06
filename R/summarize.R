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
#' @param .sort _experimental_: Default `TRUE`.
#'   If FALSE the original order of the grouping variables will be preserved.
#' @param .groups Grouping structure of the result
#'   * "drop_last": Drop the last level of grouping
#'   * "drop": Drop all groups
#'   * "keep": Keep all groups
#' @param .unpack _experimental_: Default `FALSE`. Should unnamed data frame inputs be unpacked.
#'   The user must opt in to this option as it can lead to a reduction in performance.
#'
#' @export
#' @examples
#' df <- data.table(
#'   a = 1:3,
#'   b = 4:6,
#'   c = c("a", "a", "b"),
#'   d = c("a", "a", "b")
#' )
#'
#' df %>%
#'   summarize(avg_a = mean(a),
#'             max_b = max(b),
#'             .by = c)
#'
#' df %>%
#'   summarize(avg_a = mean(a),
#'             .by = c(c, d))
summarize <- function(.df, ...,
                      .by = NULL,
                      .sort = TRUE,
                      .groups = "drop_last",
                      .unpack = FALSE) {
  if (!is_tidytable(.df)) .df <- as_tidytable(.df)

  if (is_ungrouped(.df)) {
    tt_summarize(.df, ...,
                 .by = {{ .by }},
                 .sort = .sort,
                 .groups = .groups,
                 .unpack = .unpack)
  } else if (is_grouped_df(.df)) {
    .by <- group_vars(.df)
     out <-  tt_summarize(.df, ...,
                          .by = any_of(.by),
                          .sort = .sort,
                          .groups = .groups,
                          .unpack = .unpack)

     .groups <- arg_match0(.groups, c("drop_last", "drop", "keep"))
     if (.groups == "drop_last") {
       .by <- .by[-length(.by)]
     } else if (.groups == "drop") {
       .by <- character()
     }

     group_by(out, any_of(.by))
  } else {
    abort("`summarize()` is not yet supported on a rowwise tidytable.")
  }
}

#' @export
#' @keywords internal
#' @inherit summarize
summarize. <- function(.df, ...,
                      .by = NULL,
                      .sort = TRUE,
                      .groups = "drop_last",
                      .unpack = FALSE) {
  deprecate_dot_fun()
  summarize(.df, ...,
            .by = {{ .by }},
            .sort = .sort,
            .groups = .groups,
            .unpack = .unpack)
}

tt_summarize <- function(.df, ...,
                                 .by = NULL,
                                 .sort = TRUE,
                                 .groups = "drop",
                                 .unpack = FALSE) {
  dots <- enquos(...)

  .by <- enquo(.by)

  if (length(dots) == 0) {
    # Issue #379
    out <- distinct(.df, !!.by)
  } else {
    dt_env <- get_dt_env(dots)

    dots <- prep_exprs(dots, .df, !!.by, dt_env = dt_env)

    .by <- tidyselect_names(.df, !!.by)

    if (is_true(.unpack)) {
      # https://github.com/markfairbanks/tidytable/issues/576
      j <- call2("df_list", !!!dots, .ns = "vctrs")
    } else {
      j <- call2(".", !!!dots)
    }

    dt_expr <- call2_j(.df, j, .by, .sort)

    out <- eval_tidy(dt_expr, .df, dt_env)

    out <- remove_key(out)

    out <- df_name_repair(out, "unique")
  }

  out
}

#' @export
#' @rdname summarize
summarise <- summarize

#' @export
#' @keywords internal
#' @inherit summarize
summarise. <- summarize.


