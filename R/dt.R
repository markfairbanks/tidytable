#' Pipeable data.table call
#'
#' @description
#' Pipeable data.table call.
#'
#' This function does not use data.table's modify-by-reference.
#'
#' Has experimental support for tidy evaluation for custom functions.
#'
#' @param .df A data.frame or data.table
#' @param i i position of a data.table call. See `?data.table::data.table`
#' @param j j position of a data.table call. See `?data.table::data.table`
#' @param ... Other arguments passed to data.table call. See `?data.table::data.table`
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
#' # Experimental support for tidy evaluation for custom functions
#' add_one <- function(data, col) {
#'   data %>%
#'     dt(, new_col := {{ col }} + 1)
#' }
#'
#' df %>%
#'   add_one(x)
#' @export
dt <- function(.df, i, j, ...) {
  UseMethod("dt")
}

#' @export
dt.tidytable <- function(.df, i, j, ...) {
  args <- enquos(i, j, ..., .unquote_names = FALSE, .ignore_empty = "none")

  if (all(map_lgl(args, quo_is_missing))) return(.df)

  dt_env <- get_dt_env(args)

  args <- lapply(args, quo_squash)

  dt_expr <- call2("[", quo(.df), !!!args)

  dt_expr <- call_match(dt_expr, internal_dt)

  args <- call_args(dt_expr)

  if (!is.null(args$j)) {
    is_mutate <- is_call(args$j, c(":=", "let"))
    if (is_mutate) {
      # Find cols mutated for `fast_copy()`
      mutate_exprs <- call_args(args$j)
      if (is_single_mutate(mutate_exprs)) {
        cols <- mutate_exprs[[1]]
        if (is.null(mutate_exprs[[2]])) {
          # .df[, col := NULL]
          cols <- character()
        } else if (is.symbol(cols)) {
          # .df[, x := x * 2]
          cols <- as.character(cols)
        } else {
          # .df[, "double_x" := x * 2]
          # .df[, (new_col) := x * 2] # Note: needs dt_env
          # .df[, c("x", "y") := lapply(.SD, \(x) x + 1), .SDcols = c("x", "y")]
          cols <- eval_tidy(cols, env = dt_env)
        }
      } else {
        # .df %>% dt(, let(x = 1, double_y = y * 2))
        # .df %>% dt(, let(!!col := !!col * 2))
        args$j <- prep_j_expr(args$j)
        cols <- call_args_names(args$j)
      }
      .df <- fast_copy(.df, cols)
    } else if (is_call(args$j, c(".", "list"))) {
      # .df %>% dt(, .(mean_x = mean(x)))
      # .df %>% dt(, .(!!col := mean(!!col)))
      args$j <- prep_j_expr(args$j)
    }

    dt_expr <- call2("[", !!!args)

    # Only add empty `[` when using mutate
    if (is_mutate) {
      dt_expr <- call2("[", dt_expr)
    }
  }

  eval_tidy(dt_expr, env = dt_env)
}

#' @export
dt.data.frame <- function(.df, i, j, ...) {
  .df <- as_tidytable(.df)
  dt(.df, {{ i }}, {{ j }}, ...)
}

# Checks if j is a single call to `:=` or let
is_single_mutate <- function(mutate_exprs) {
  no_names <- !any(have_name(mutate_exprs))
  no_walrus <- !any(map_lgl(mutate_exprs, is_call, ":="))
  has_length(mutate_exprs, 2) && no_names && no_walrus
}

# Allow unquoting names in j position & allow using let
# Ex: df %>% dt(, let({{ col }} := {{ col }} * 2))
# Ex: df %>% dt(, .(!!col := mean(!!col)))
prep_j_expr <- function(j) {
  if (is_call(j, "let")) {
    j[[1]] <- expr(`:=`)
  }
  j_call <- call_name(j)
  j_exprs <- call_args(j)
  use_walrus <- map_lgl(j_exprs, is_call, ":=")
  if (any(use_walrus)) {
    walrus_exprs <- j_exprs[use_walrus]
    walrus_exprs <- map(walrus_exprs, ~ call_args(.x))
    walrus_names <- map_chr(walrus_exprs, ~ as.character(.x[[1]]))
    walrus_exprs <- map(walrus_exprs, ~ .x[[2]])
    j_exprs[use_walrus] <- walrus_exprs
    names(j_exprs)[use_walrus] <- walrus_names

    j <- call2(j_call, !!!j_exprs)
  }
  j
}

# Dummy function with `[.data.table` arguments
# For use with call_match in `dt()`
internal_dt <- function(x, i, j, by, keyby, with = TRUE,
                        nomatch = NA,
                        mult = "all",
                        roll = FALSE,
                        rollends = if (roll=="nearest") c(TRUE,TRUE)
                        else if (roll>=0) c(FALSE,TRUE)
                        else c(TRUE,FALSE),
                        which = FALSE,
                        .SDcols,
                        verbose = FALSE,
                        allow.cartesian = FALSE,
                        drop = NULL, on = NULL, env = NULL) {
  abort("For internal call_match only.")
}
