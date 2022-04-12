#' Pipeable data.table call
#'
#' @description
#' Pipeable data.table call
#'
#' Note: This function does not use data.table's modify-by-reference
#'
#' @param .df A data.frame or data.table
#' @param ... Arguments passed to data.table call. See ?data.table::`[.data.table`
#'
#' @examples
#' df <- tidytable(
#'   x = 1:3,
#'   y = 4:6,
#'   z = c("a", "a", "b")
#' )
#'
#' df %>%
#'   dt(, double_x := x * 2) %>%
#'   dt(order(-double_x))
#' @export
dt <- function(.df, ...) {
  UseMethod("dt")
}

#' @export
dt.tidytable <- function(.df, ...) {
  # TODO: Add test that let() doesn't modify-by-reference
    ## once 1.14.4 is released
  dots <- enquos(..., .unquote_names = FALSE)
  dt_env <- get_dt_env(dots)
  dots <- lapply(dots, quo_squash)
  dots_names <- names(dots)

  if (length(dots) > 1 || "j" %chin% dots_names) {
    if ("j" %chin% dots_names) {
      j <- dots[["j"]]
    } else {
      j <- dots[[2]]
    }

    if (is_call(j, c(":=", "let"))) {
      mut_exprs <- j[-1]
      if (length(mut_exprs) == 2 && is.null(names(mut_exprs))) {
        col_name <- mut_exprs[[1]]
        if (is_call(col_name, "(")) {
          # .df[, (new_col) := x * 2]
          col_name <- eval_tidy(col_name[[-1]], env = dt_env)
        } else if (is.symbol(col_name)) {
          # .df[, x := x * 2]
          col_name <- as.character(col_name)
        } else {
          # .df[, "double_x" := x * 2]
          # .df[, c("x", "y") := lapply(.SD, \(x) x + 1), .SDcols = c("x", "y")]
          col_name <- eval_tidy(col_name)
        }
        .df <- fast_copy(.df, col_name)
      } else {
        # .df[, let(x = 1, double_y = y * 2)]
        col_names <- names(mut_exprs)
        .df <- fast_copy(.df, col_names)
      }
    }
  }

  dt_expr <- call2("[", quo(.df), !!!dots)

  # Only add empty `[` when using mutate
  if (exists("mut_exprs", envir = current_env())) {
    dt_expr <- call2("[", dt_expr)
  }

  eval_tidy(dt_expr, env = dt_env)
}

#' @export
dt.data.frame <- function(.df, ...) {
  .df <- as_tidytable(.df)
  dt(.df, ...)
}
