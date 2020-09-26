#' Add/modify/delete columns
#'
#' @description
#' With `mutate.()` you can do 3 things:
#' * Add new columns
#' * Modify existing columns
#' * Delete columns
#'
#' @param .df A data.frame or data.table
#' @param ... Columns to add/modify
#' @param .by Columns to group by
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
#'   mutate.(double_a = a * 2,
#'           a_plus_b = a + b)
#'
#' test_df %>%
#'   mutate.(double_a = a * 2,
#'           avg_a = mean(a),
#'           .by = c)
mutate. <- function(.df, ..., .by = NULL) {
  UseMethod("mutate.")
}

#' @export
mutate..data.frame <- function(.df, ..., .by = NULL) {

  .df <- as_tidytable(.df)
  .df <- shallow(.df)

  dots <- enquos(...)
  .by <- enquo(.by)

  if (quo_is_null(.by)) {
    # Faster version if there is no "by" provided
    all_names <- names(dots)

    for (i in seq_along(dots)) {

      .col_name <- all_names[[i]]
      .val <- dots[[i]]

      data_env <- env(quo_get_env(.val), .df = .df)

      # Prevent modify-by-reference if the column already exists in the data.table
      # Fixes cases when user supplies a single value ex. 1, -1, "a"
      # !quo_is_null(val) allows for columns to be deleted using mutate.(.df, col = NULL)
      if (.col_name %in% names(.df) && !quo_is_null(.val)) {

        eval_quo(
          .df[, !!.col_name := vctrs::vec_recycle(!!.val, .N)],
          new_data_mask(data_env), caller_env()
        )

      } else {

        eval_quo(
          .df[, !!.col_name := !!.val],
          new_data_mask(data_env), caller_env()
        )
      }
    }
  } else {
    # Faster with "by", since the "by" call isn't looped multiple times for each column added
    .by <- select_vec_chr(.df, !!.by)

    needs_copy <- any(vec_in(names(dots), names(.df)))

    if (needs_copy) .df <- copy(.df)

    data_env <- env(quo_get_env(dots[[1]]), .df = .df, .by = .by)

    eval_quo(
      .df[, ':='(!!!dots), by = .by],
      new_data_mask(data_env), env = caller_env()
    )

  }
  .df[]
}

#' @export
#' @rdname dt_verb
#' @inheritParams mutate.
dt_mutate <- function(.df, ..., .by = NULL) {
  deprecate_stop("0.5.2", "tidytable::dt_mutate()", "mutate.()")

  mutate.(.df, ..., .by = {{ .by }})
}
