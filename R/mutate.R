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

  if (length(dots) == 0) return(.df)

  if (quo_is_null(.by)) {
    # Faster version if there is no ".by" provided
    all_names <- names(dots)

    data_env <- env(quo_get_env(dots[[1]]), .df = .df)

    for (i in seq_along(dots)) {

      .col_name <- all_names[[i]]
      .val <- dots[[i]]

      # Prevent modify-by-reference if the column already exists in the data.table
        # Fixes case when user supplies a single value ex. 1, -1, "a"
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

    needs_copy <- any(names(dots) %in% names(.df))

    if (needs_copy) .df <- copy(.df)

    data_env <- env(quo_get_env(dots[[1]]), .df = .df, .by = .by)

    # Check for NULL inputs so columns can be deleted
    null_flag <- map_lgl.(dots, quo_is_null)

    if (any(null_flag)) {
      null_dots <- dots[null_flag]

      dots <- dots[!null_flag]

      eval_quo(
        .df[, ':='(!!!null_dots)],
        new_data_mask(data_env), env = caller_env()
      )
    }

    if (length(dots) > 0) {

      assign <- map2.(syms(names(dots)), dots, ~ call2("<-", .x, .y))
      output <- call2("list", !!!syms(names(dots)))
      expr <- call2("{", !!!assign, output)
      j <- call2(":=", call2("c", !!!names(dots)), expr)

      eval_quo(
        .df[, !!j, by = .by],
        new_data_mask(data_env), env = caller_env()
      )
    }

  }
  .df[]
}
