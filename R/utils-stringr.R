# These functions are for internal use only

str_c <- function(..., sep = "", collapse = NULL) {
  paste(..., sep = sep, collapse = collapse)
}

str_detect <- function(string, pattern) {
  grepl(pattern, string)
}

str_ends <- function(string, pattern) {
  endsWith(string, pattern)
}

str_replace <- function(string, pattern, replacement) {
  sub(pattern, replacement, string)
}

str_replace_all <- function(string, pattern, replacement) {
  gsub(pattern, replacement, string)
}

str_starts <- function(string, pattern) {
  startsWith(string, pattern)
}

str_extract <- function(x, pattern) {
  regmatches(x, regexpr(pattern, x))
}

