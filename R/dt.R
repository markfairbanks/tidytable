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
#'     dt(, new_col := {{ col }} + 1)
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

  if (has_length(dots, 0)) return(.df)

  dt_env <- get_dt_env(dots)

  dots <- lapply(dots, quo_squash)

  call <- call2("[", quo(.df), !!!dots)

  call <- call_match(call, internal_dt_subset)

  args <- call_args(call)

  j <- args$j
  if (!is.null(j)) {
    if (is_call(j, c(":=", "let"))) {
      mutate_exprs <- call_args(j)
      if (is_basic_mutate(mutate_exprs)) {
        col_name <- mutate_exprs[[1]]
        if (is.null(mutate_exprs[[2]])) {
          # .df[, col := NULL]
          col_name <- character()
        } else if (is.symbol(col_name)) {
          # .df[, x := x * 2]
          col_name <- as.character(col_name)
        } else {
          # .df[, "double_x" := x * 2]
          # .df[, (new_col) := x * 2] # Note: needs dt_env
          # .df[, c("x", "y") := lapply(.SD, \(x) x + 1), .SDcols = c("x", "y")]
          col_name <- eval_tidy(col_name, env = dt_env)
        }
        .df <- fast_copy(.df, col_name)
      } else {
        # .df %>% dt(, let(x = 1, double_y = y * 2))
        # .df %>% dt(, let(!!col := !!col * 2))
        j <- prep_j_expr(j)
        args$j <- j
        col_names <- call_args_names(j)
        .df <- fast_copy(.df, col_names)
      }
    } else if (is_call(j, c(".", "list"))) {
      # .df %>% dt(, .(mean_x = mean(x)))
      # .df %>% dt(, .(!!col := mean(!!col)))
      j <- prep_j_expr(j)
      args$j <- j
    }
  }

  dt_expr <- call2("[", !!!args)

  # Only add empty `[` when using mutate
  if (env_has(current_env(), "mutate_exprs")) {
    dt_expr <- call2("[", dt_expr)
  }

  eval_tidy(dt_expr, env = dt_env)
}

#' @export
dt.data.frame <- function(.df, ...) {
  .df <- as_tidytable(.df)
  dt(.df, ...)
}

# Checks if j is a single call to `:=` or let
is_basic_mutate <- function(mutate_exprs) {
  no_names <- !any(have_name(mutate_exprs))
  no_walrus <- !any(map_lgl(mutate_exprs, is_call, ":="))
  has_length(mutate_exprs, 2) && no_names && no_walrus
}

# Allow unquoting names in j position & allow using let
#   Ex: df %>% dt(, let({{ col }} := {{ col }} * 2))
#   Ex: df %>% dt(, .(!!col := mean(!!col)))
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

internal_dt_subset <- function(x, i, j, by, keyby, with = TRUE,
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
                               drop = NULL, on = NULL) {
  abort("For internal call_match only.")
}
