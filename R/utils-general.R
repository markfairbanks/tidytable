# Creates a shallow copy to prevent modify-by-reference
shallow <- function(x, cols = names(x), reset_class = FALSE) {
  stopifnot(is.data.table(x), all(cols %in% names(x)))
  ans = vector("list", length(cols))
  setattr(ans, 'names', data.table::copy(cols))
  for (col in cols)
    ans[[col]] = x[[col]]
  setDT(ans)
  class = if (!reset_class) data.table::copy(class(x))
  else c("data.table", "data.frame")
  setattr(ans, 'class', class)
  ans[]
}

