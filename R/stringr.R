# These functions are for internal use only

dt_str_c <- function(..., sep = "", collapse = NULL) {
  paste(..., sep = sep, collapse = collapse)
}

dt_str_detect <- function(string, pattern) {
  grepl(pattern, string)
}

dt_str_ends <- function(string, pattern) {
  endsWith(string, pattern)
}

dt_str_replace <- function(string, pattern, replacement) {
  sub(pattern, replacement, string)
}

dt_str_replace_all <- function(string, pattern, replacement) {
  gsub(pattern, replacement, string)
}

dt_str_starts <- function(string, pattern) {
  startsWith(string, pattern)
}


