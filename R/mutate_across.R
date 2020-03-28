#' Mutate multiple columns simultaneously
#'
#' @description
#' Mutate multiple columns simultaneously.
#'
#' Supports enhanced selection.
#'
#' @md
#'
#' @param .data A data.frame or data.table
#' @param .cols vector `c()` of bare column names for `mutate_across.()` to use. Supports enhanced selection.
#' @param .funs Functions to pass. Can pass a list of functions.
#' @param ... Other arguments for the passed function
#' @param by Columns to group by
#'
#' @export
#'
#' @examples
#' example_dt <- data.table::data.table(
#'   x = c(1,1,1),
#'   y = c(2,2,2),
#'   z = c("a", "a", "b"))
#'
#' example_dt %>%
#'   mutate_across.(is.numeric, as.character)
#'
#' example_dt %>%
#'   mutate_across.(c(x, y), ~ .x * 2)
#'
#' example_dt %>%
#'   mutate_across.(everything.(), as.character)
#'
#' example_dt %>%
#'   mutate_across.(c(x, y), list(new = ~ .x * 2,
#'                                another = ~ .x + 7))
mutate_across. <- function(.data, .cols = everything.(), .funs, ..., by = NULL) {
  UseMethod("mutate_across.")
}

#' @export
mutate_across..tidytable <- function(.data, .cols = everything.(), .funs, ..., by = NULL) {

  .cols <- enexpr(.cols)
  .cols <- as.character(vec_selector(.data, !!.cols))

  by <- enexpr(by)
  by <- vec_selector_by(.data, !!by)

  .data <- shallow(.data)

  if (!is.list(.funs)) {
    if (length(.cols) > 0) {
      eval_expr(
        .data[, (.cols) := map.(.SD, .funs, ...), .SDcols = .cols, by = !!by]
      )
    } else {
      .data
    }
  } else {

    if (!is_named(.funs)) abort("functions passed in a list must be named")

    new_names <- names(.funs)

    for (i in seq_along(new_names)) {
      new_cols <-  paste0(.cols, "_", new_names[[i]])

      eval_expr(
        .data[, (new_cols) := map.(.SD, .funs[[i]]), .SDcols = .cols, by = !!by]
      )
    }
  }
  .data[]
}

#' @export
mutate_across..data.frame <- function(.data, .cols = everything.(), .funs, ..., by = NULL) {
  .data <- as_tidytable(.data)
  .cols <- enexpr(.cols)
  by <- enexpr(by)

  mutate_across.(.data, !!.cols, .funs, ..., by = !!by)
}

#' @export
#' @rdname mutate_across.
dt_mutate_across <- mutate_across.
