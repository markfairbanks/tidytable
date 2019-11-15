enlist_dots <- function(...) {
  as.list(substitute(list(...)))[-1]
}
