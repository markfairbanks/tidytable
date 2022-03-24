# _locs: Get column locations (as an integer vector)
# _names: Get column names (as a character vector)
# _syms: Get column names (as a list of symbols)

tidyselect_locs <- function(.df, ...) {
  eval_select(expr(c(...)), .df)
}

tidyselect_names <- function(.df, ...) {
  names(tidyselect_locs(.df, ...))
}

tidyselect_syms <- function(.df, ...) {
  syms(tidyselect_names(.df, ...))
}
