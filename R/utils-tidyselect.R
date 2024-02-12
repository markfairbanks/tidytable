# _locs: Get column locations (as an integer vector)
# _names: Get column names (as a character vector)
# _syms: Get column names (as a list of symbols)

tidyselect_locs <- function(.df, ..., .allow_rename = TRUE) {
  eval_select(expr(c(...)), .df, allow_rename = .allow_rename)
}

tidyselect_names <- function(.df, ..., .allow_rename = TRUE) {
  names(tidyselect_locs(.df, ..., .allow_rename = .allow_rename))
}

tidyselect_syms <- function(.df, ..., .allow_rename = TRUE) {
  syms(tidyselect_names(.df, ..., .allow_rename = .allow_rename))
}
