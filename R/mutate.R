#' Mutate
#'
#' @description
#' Add new columns or modify existing ones
#'
#' @param .data A data.frame or data.table
#' @param ... Columns to add/modify
#' @param by Columns to group by
#'
#' @md
#' @export
#'
#' @examples
#' example_dt <- data.table::data.table(
#'   a = c(1,2,3),
#'   b = c(4,5,6),
#'   c = c("a","a","b"))
#'
#' example_dt %>%
#'   mutate.(double_a = a * 2,
#'           a_plus_b = a + b)
#'
#' example_dt %>%
#'   mutate.(double_a = a * 2,
#'           avg_a = mean(a),
#'           by = c)
mutate. <- function(.data, ..., by = NULL) {
  UseMethod("mutate.")
}

#' @export
mutate..tidytable <- function(.data, ..., by = NULL) {

  dots <- enexprs(...)
  by <- enexpr(by)
  .data <- shallow(.data)

  if (is.null(by)) {
    # Faster version if there is no "by" provided
    all_names <- names(dots)

    for (i in seq_along(dots)) {

      .col_name <- all_names[[i]]
      val <- dots[i][[1]]

      # Prevent modify-by-reference if the column already exists in the data.table
      # Steps: Create new col with random name, delete original col, rename random back to original, reorder
      # Fixes cases when user supplies a single value ex. 1, -1, "a"
      # !is.null(val) allows for columns to be deleted using mutate.(.data, col = NULL)
      if (.col_name %in% names(.data) && !is.null(val)) {
        col_order <- unique(c(names(.data), .col_name))

        eval_expr(
          .data[, ':='(.new_col, !!val)][, !!.col_name := NULL]
        )

        setnames(.data, ".new_col", .col_name)
        setcolorder(.data, col_order)
      } else {
        eval_expr(
          .data[, ':='(!!.col_name, !!val)]
        )
      }
    }
  } else {
    # Faster with "by", since the "by" call isn't looped multiple times for each column added
    by <- vec_selector_by(.data, !!by)

    eval_expr(
      .data[, ':='(!!!dots), by = !!by]
    )
  }
  .data[]
}

#' @export
mutate..data.frame <- function(.data, ..., by = NULL) {
  .data <- as_tidytable(.data)
  by <- enexpr(by)

  mutate.(.data, ..., by = !!by)
}


#' @export
#' @rdname mutate.
dt_mutate <- mutate.
