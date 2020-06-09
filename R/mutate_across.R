#' Mutate multiple columns simultaneously
#'
#' @description
#' Mutate multiple columns simultaneously.
#'
#' @param .df A data.frame or data.table
#' @param .cols vector `c()` of unquoted column names. `tidyselect` compatible.
#' @param .fns Functions to pass. Can pass a list of functions.
#' @param ... Other arguments for the passed function
#' @param .by Columns to group by
#' @param by This argument has been renamed to .by and is deprecated
#'
#' @export
#' @md
#'
#' @examples
#' test_df <- data.table(
#'   x = c(1,1,1),
#'   y = c(2,2,2),
#'   z = c("a", "a", "b"))
#'
#' test_df %>%
#'   mutate_across.(where(is.numeric), as.character)
#'
#' test_df %>%
#'   mutate_across.(c(x, y), ~ .x * 2)
#'
#' test_df %>%
#'   mutate_across.(everything(), as.character)
#'
#' test_df %>%
#'   mutate_across.(c(x, y), list(new = ~ .x * 2,
#'                                another = ~ .x + 7))
mutate_across. <- function(.df, .cols = everything(), .fns, ..., .by = NULL, by = NULL) {
  UseMethod("mutate_across.")
}

#' @export
mutate_across..data.frame <- function(.df, .cols = everything(), .fns, ..., .by = NULL, by = NULL) {

  .df <- as_tidytable(.df)

  .cols <- select_vec_chr(.df, {{ .cols }})

  .by <- check_dot_by(enquo(.by), enquo(by), "mutate_across.")
  .by <- select_vec_chr(.df, !!.by)

  .df <- shallow(.df)

  if (!is.list(.fns)) {
    if (length(.cols) > 0) {
      eval_quo(
        .df[, (.cols) := map.(.SD, .fns, ...), .SDcols = !!.cols, by = !!.by],
      )
    } else {
      .df
    }
  } else {

    if (!is_named(.fns)) abort("functions passed in a list must be named")

    new_names <- names(.fns)

    for (i in seq_along(new_names)) {
      new_cols <-  paste0(.cols, "_", new_names[[i]])

      eval_quo(
        .df[, (new_cols) := map.(.SD, .fns[[i]]), .SDcols = !!.cols, by = !!.by],
      )
    }
  }
  .df[]
}

#' @export
#' @rdname mutate_across.
dt_mutate_across <- function(.df, .cols = everything(), .fns, ..., .by = NULL, by = NULL) {
  deprecate_soft("0.5.2", "tidytable::dt_mutate_across()", "mutate_across.()")

  .by <- check_dot_by(enquo(.by), enquo(by))
  mutate_across.(.df, .cols = {{ .cols }}, .fns, ..., .by = {{ .by }})
}
