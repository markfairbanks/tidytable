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
#'   c = c("a","a","b")
#' )
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

  .by <- enquo(.by)

  dots <- enquos(...)
  if (length(dots) == 0) return(.df)

  mask <- build_data_mask(dots)

  if (quo_is_null(.by)) {
    for (i in seq_along(dots)) {
      dots_i <- prep_exprs(dots[i], .df, !!.by)
      dots_i_names <- names(dots_i)

      dots_i <- map2.(dots_i, dots_i_names, ~ mutate_prep(.df, .x, .y))

      j <- expr(':='(!!!dots_i))

      dt_expr <- call2_j(.df, j)

      .df <- eval_tidy(dt_expr, mask, caller_env())
    }
  } else {
    if (length(dots) > 1) {
      across_flag <- map_lgl.(dots[-1], quo_is_call, "across.")

      if (any(across_flag)) {
        abort("across.() can only be used in the first position of mutate.()
              when `.by` is used.")
      }
    }

    dots <- prep_exprs(dots, .df, !!.by)

    .by <- select_vec_chr(.df, !!.by)

    needs_copy <- any(names(dots) %in% names(.df))
    if (needs_copy) .df <- copy(.df)

    # Check for NULL inputs so columns can be deleted
    null_flag <- map_lgl.(dots, is_null)
    if (any(null_flag)) {
      null_dots <- dots[null_flag]

      dots <- dots[!null_flag]

      j <- call2(":=", !!!null_dots)
      dt_expr <- call2_j(.df, j)

      .df <- eval_tidy(dt_expr, mask, caller_env())
    }

    if (length(dots) > 0) {
      assign <- map2.(syms(names(dots)), dots, ~ call2("<-", .x, .y))
      output <- call2("list", !!!syms(names(dots)))
      expr <- call2("{", !!!assign, output)
      j <- call2(":=", call2("c", !!!names(dots)), expr)
      dt_expr <- call2_j(.df, j, .by)

      .df <- eval_tidy(dt_expr, mask, caller_env())
    }

  }
  .df[]
}

# vec_recycle() prevents modify-by-reference if the column already exists in the data.table
# Fixes case when user supplies a single value ex. 1, -1, "a"
# !is_null(val) allows for columns to be deleted using mutate.(.df, col = NULL)
mutate_prep <- function(data, dot, dot_name) {
  if (dot_name %in% names(data) && !is_null(dot)) {
    dot <- call2("vec_recycle", dot, expr(.N), .ns = "vctrs")
  }
  dot
}
