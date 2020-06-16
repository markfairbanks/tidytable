#' Mutate
#'
#' @description
#' Add new columns or modify existing ones
#'
#' @param .df A data.frame or data.table
#' @param ... Columns to add/modify
#' @param .by Columns to group by
#' @param by This argument has been renamed to .by and is deprecated
#'
#' @md
#' @export
#'
#' @examples
#' test_df <- data.table(
#'   a = c(1,2,3),
#'   b = c(4,5,6),
#'   c = c("a","a","b"))
#'
#' test_df %>%
#'   mutate.(double_a = a * 2,
#'           a_plus_b = a + b)
#'
#' test_df %>%
#'   mutate.(double_a = a * 2,
#'           avg_a = mean(a),
#'           .by = c)
mutate. <- function(.df, ..., .by = NULL, by = NULL) {
  UseMethod("mutate.")
}

#' @export
mutate..data.frame <- function(.df, ..., .by = NULL, by = NULL) {

  .df <- as_tidytable(.df)
  .df <- shallow(.df)

  dots <- enquos(...)
  .by <- check_dot_by(enquo(.by), enquo(by), "mutate.")

  if (quo_is_null(.by)) {
    # Faster version if there is no "by" provided
    all_names <- names(dots)

    for (i in seq_along(dots)) {

      .col_name <- all_names[[i]]
      val <- dots[i][[1]]

      # Prevent modify-by-reference if the column already exists in the data.table
      # Fixes cases when user supplies a single value ex. 1, -1, "a"
      # !quo_is_null(val) allows for columns to be deleted using mutate.(.df, col = NULL)
      if (.col_name %in% names(.df) && !quo_is_null(val)) {

        eval_quo(
          .df[, !!.col_name := vec_recycle(!!val, .N)]
        )

      } else {

        eval_quo(
          .df[, !!.col_name := !!val]
        )
      }
    }
  } else {
    # Faster with "by", since the "by" call isn't looped multiple times for each column added
    .by <- select_vec_chr(.df, !!.by)

    needs_copy <- any(vec_in(names(dots), names(.df)))

    if (needs_copy) .df <- copy(.df)

    eval_quo(
      .df[, ':='(!!!dots), by = .by]
    )

  }
  .df[]
}

#' @export
#' @rdname mutate.
dt_mutate <- function(.df, ..., .by = NULL, by = NULL) {
  deprecate_soft("0.5.2", "tidytable::dt_mutate()", "mutate.()")

  .by <- check_dot_by(enquo(.by), enquo(by))
  mutate.(.df, ..., .by = {{ .by }})
}
