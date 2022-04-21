#' Pipeable data.table call
#'
#' @description
#' Pipeable data.table call.
#'
#' Has *experimental* support for tidy evaluation.
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
#'
#' # Experimental support for tidy evaluation
#' add_one <- function(data, col) {
#'   data %>%
#'     dt(, {{ col }} := {{ col }} + 1)
#' }
#'
#' df %>%
#'   add_one(x)
#' @export
dt <- function(.df, ...) {
  UseMethod("dt")
}

#' @export
dt.tidytable <- function(.df, ...) {
  dots <- enquos(..., .unquote_names = FALSE, .ignore_empty = "none")
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
      mut_exprs <- as.list(j[-1])
      if (length(mut_exprs) == 2 && is.null(names(mut_exprs))) {
        col_name <- mut_exprs[[1]]
        if (is.null(mut_exprs[[2]])) {
          # .df[, col := NULL]
          col_name <- character()
        } else if (is_call(col_name, "(")) {
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
        use_walrus <- map_lgl.(mut_exprs, is_call, ":=")
        if (any(use_walrus)) {
          # .df %>% dt(, let(!!col := !!col * 2))
          j <- prep_j_expr(mut_exprs, use_walrus, ":=")
          dots <- replace_j_dot(dots, dots_names, j)
        }
        col_names <- names(as.list(j[-1]))
        .df <- fast_copy(.df, col_names)
      }
    } else if (is_call(j, c(".", "list"))) {
      summarize_exprs <- as.list(j[-1])
      use_walrus <- map_lgl.(summarize_exprs, is_call, ":=")
      if (any(use_walrus)) {
        # .df %>% dt(, .(!!col := mean(!!col)))
        j <- prep_j_expr(summarize_exprs, use_walrus, ".")
        dots <- replace_j_dot(dots, dots_names, j)
      }
    }
  }

  dt_expr <- call2("[", quo(.df), !!!dots)

  # Only add empty `[` when using mutate
  if (exists("mut_exprs", current_env())) {
    dt_expr <- call2("[", dt_expr)
  }

  eval_tidy(dt_expr, env = dt_env)
}

prep_j_expr <- function(j_exprs, use_walrus, j_call) {
  walrus_exprs <- j_exprs[use_walrus]
  walrus_exprs <- map.(walrus_exprs, ~ as.list(.x[-1]))
  walrus_names <- map_chr.(walrus_exprs, ~ as.character(.x[[1]]))
  walrus_exprs <- map.(walrus_exprs, ~ .x[[2]])
  j_exprs[use_walrus] <- walrus_exprs
  names(j_exprs)[use_walrus] <- walrus_names

  new_j <- call2(j_call, !!!j_exprs)
  new_j
}

replace_j_dot <- function(dots, dots_names, j) {
  if ("j" %chin% dots_names) {
    dots[["j"]] <- j
  } else {
    dots[[2]] <- j
  }
  dots
}

#' @export
dt.data.frame <- function(.df, ...) {
  .df <- as_tidytable(.df)
  dt(.df, ...)
}
