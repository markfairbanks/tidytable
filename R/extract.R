extract. <- function(.df, ...) {
  UseMethod("extract.")
}

extract..data.frame <- function(.df, col, into,
                    regex = "([[:alnum:]]+)",
                    remove = TRUE,
                     ...) {
  
  .df <- as_tidytable(.df)
  .df <- shallow(.df)

  if (missing(col)) abort("col is missing and must be supplied")
  if (missing(into)) abort("into is missing and must be supplied")

  col <- select_vec_chr(.df, !!enquo(col))

  groups <- str_extract_groups(.df[[col]], regex)

  if (length(groups) != length(into)) {
    abort(
      glue("`regex` pattern should define {length(into)} groups; {length(groups)} found.")
    )
  }
  .df[, (into) := groups]
  
  if (remove) .df[, (col) := NULL]

  .df[]
}

str_extract_groups <- function(string, pattern){
  groups <- regexpr(pattern, string, perl = TRUE)
  start <- attr(groups, "capture.start")
  end <- start + attr(groups, "capture.length") - 1L

  # in order to force substr to return NA when No match is found
  # set the start and end to NA
  none_found <- start == -1 
  start[none_found] <- NA
  end[none_found] <- NA

  map.(1:ncol(start), ~ substr(string, start[, .x], end[, .x]))
}