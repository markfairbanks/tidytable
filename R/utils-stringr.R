# These functions are for internal use only
# Functions must be named verb.() (#91)

str_c. <- function(..., sep = "", collapse = NULL) {
  paste(..., sep = sep, collapse = collapse)
}

str_detect. <- function(string, pattern, fixed = FALSE, perl = FALSE) {
  grepl(pattern, string, fixed = fixed, perl = perl)
}

str_ends. <- function(string, pattern) {
  endsWith(string, pattern)
}

str_extract. <- function(x, pattern, fixed = FALSE, perl = FALSE) {
  regmatches(x, regexpr(pattern, x, fixed = fixed, perl = perl))
}

str_extract_all. <- function(x, pattern, fixed = FALSE, perl = FALSE) {
  regmatches(x, gregexpr(pattern, x, fixed = fixed, perl = perl))
}

str_replace. <- function(string, pattern, replacement, fixed = FALSE, perl = FALSE) {
  sub(pattern, replacement, string, fixed = fixed, perl = perl)
}

str_replace_all. <- function(string, pattern, replacement, fixed = FALSE, perl = FALSE) {

  if (missing(replacement)) {

    replacement <- unname(pattern)
    pattern <- names(pattern)

  } else if (length(pattern) != length(replacement)) {

    if (length(pattern) > 1 & length(replacement) > 1) {

      stop("replacement has to have the same length as pattern")

    } else {

      if (length(replacement) == 1) replacement <- rep(replacement, length(pattern))
      else pattern <- rep(pattern, length(replacement))

    }
  }

  for (i in 1:length(pattern)) {
    string <- gsub(pattern[[i]], replacement[[i]], string,  perl = perl, fixed = fixed)
  }

  string
}

str_starts. <- function(string, pattern) {
  startsWith(string, pattern)
}

