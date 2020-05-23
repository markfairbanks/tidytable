#' Mutate multiple columns simultaneously
#'
#' @description
#' Mutate multiple columns simultaneously.
#'
#' @param .df A data.frame or data.table
#' @param .cols vector `c()` of unquoted column names
#' `tidyselect` compatible.
#' @param .fns Functions to pass. Can pass a list of functions.
#' @param ... Other arguments for the passed function
#' @param by Columns to group by
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
mutate_across. <- function(.df, .cols = everything(), .fns, ..., by = NULL) {
  UseMethod("mutate_across.")
}

#' @export
mutate_across..data.frame <- function(.df, .cols = everything(), .fns, ..., by = NULL) {

  .df <- as_tidytable(.df)

  .cols <- select_vec_chr(.df, {{ .cols }})

  by <- select_vec_by(.df, {{ by }})

  .df <- shallow(.df)

  if (!is.list(.fns)) {
    if (length(.cols) > 0) {
      eval_quo(
        .df[, (.cols) := eval_quo(map.(.SD, .fns, ...), .SD), .SDcols = .cols, by = !!by],
        .df)
    } else {
      .df
    }
  } else {

    if (!is_named(.fns)) abort("functions passed in a list must be named")

    new_names <- names(.fns)

    for (i in seq_along(new_names)) {
      new_cols <-  paste0(.cols, "_", new_names[[i]])

      eval_quo(
        .df[, (new_cols) := eval_quo(map.(.SD, .fns[[i]]), .SD), .SDcols = .cols, by = !!by],
        .df)
    }
  }
  .df[]
}

#' @export
#' @rdname mutate_across.
dt_mutate_across <- mutate_across.
