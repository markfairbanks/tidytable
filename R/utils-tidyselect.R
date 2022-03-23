# _locs: Get column locations (as an integer vector)
# _names: Get column names (as a character vector)
# _syms: Get column names (as a list of symbols)

tidyselect_locs <- function(.df, ..., .env = caller_env()) {
  dots <- enexprs(...)
  eval_select(expr(c(!!!dots)), .df, env = .env)
}

tidyselect_names <- function(.df, ..., .env = caller_env()) {
  names(tidyselect_locs(.df, ..., .env = .env))
}

tidyselect_syms <- function(.df, ..., .env = caller_env()) {
  syms(tidyselect_names(.df, ..., .env = .env))
}
