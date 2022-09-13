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
#' @param .sort _experimental_: Default TRUE.
#'   If FALSE the original order of the grouping variables will be preserved.
#' @param .groups Grouping structure of the result
#'   * "drop_last": Drop the last level of grouping
#'   * "drop": Drop all groups
#'   * "keep": Keep all groups
#'
#' @export
#' @md
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
                       .groups = "drop_last") {
  UseMethod("summarize")
}

#' @export
summarize.tidytable <- function(.df, ..., .by = NULL, .sort = TRUE) {
  dots <- enquos(...)

  .by <- enquo(.by)

  if (length(dots) == 0) {
    # Issue #379
    out <- distinct(.df, !!.by)
  } else {
    dt_env <- get_dt_env(dots)

    dots <- prep_exprs(dots, .df, !!.by, dt_env = dt_env)

    .by <- tidyselect_names(.df, !!.by)

    j <- call2(".", !!!dots)

    dt_expr <- call2_j(.df, j, .by, .sort)

    out <- eval_tidy(dt_expr, .df, dt_env)

    setkey(out, NULL)

    out <- df_name_repair(out, .name_repair = "unique")
  }

  out
}

#' @export
summarize.grouped_tt <- function(.df, ...,
                                 .by = NULL,
                                 .sort = TRUE,
                                 .groups = "drop_last") {
  check_by({{ .by }})
  .by <- group_vars(.df)
  out <- ungroup(.df)
  out <- summarize(out, ..., .by = all_of(.by), .sort = .sort)

  .groups <- arg_match0(.groups, c("drop_last", "drop", "keep"))
  if (.groups == "drop_last") {
    .by <- .by[-length(.by)]
  } else if (.groups == "drop") {
    .by <- character()
  }

  # summarize drop attributes in data.table - need to regroup at the end
  group_by(out, all_of(.by))
}

#' @export
summarize.data.frame <- function(.df, ..., .by = NULL, .sort = TRUE) {
  .df <- as_tidytable(.df)
  summarize(.df, ..., .by = {{ .by }}, .sort = .sort)
}

#' @export
#' @rdname summarize
summarise <- summarize

#' @export summarize.
#' @keywords internal
#' @usage
#' summarize(.df, ..., .by = NULL, .sort = TRUE, .groups = "drop_last")
#' @inherit summarize title description params examples
summarize. <- function(.df, ..., .by = NULL, .sort = TRUE, .groups = "drop_last") {
  UseMethod("summarize.")
}

#' @exportS3Method summarize. data.frame
summarize..data.frame <- function(.df, ..., .by = NULL, .sort = TRUE, .groups = "drop_last") {
  summarize(.df, ..., .by = {{ .by }}, .sort = .sort)
}

#' @export summarise.
#' @keywords internal
#' @usage
#' summarise(.df, ..., .by = NULL, .sort = TRUE, .groups = "drop_last")
#' @inherit summarise title description params examples
summarise. <- function(.df, ..., .by = NULL, .sort = TRUE, .groups = "drop_last") {
  UseMethod("summarise.")
}

#' @exportS3Method summarise. data.frame
summarise..data.frame <- function(.df, ..., .by = NULL, .sort = TRUE, .groups = "drop_last") {
  summarize(.df, ..., .by = {{ .by }}, .sort = .sort)
}
