# Fast copying
# Deep copy of columns that are being overwritten.
# Only does a shallow copy of the other columns.
# Faster than running `copy()` on an entire data frame.
fast_copy <- function(x, cols = character()) {
  if (length(cols) == 0) {
    return(shallow(x))
  }

  needs_copy <- names(x) %in% cols

  if (any(needs_copy)) {
    out <- as.list(x)
    out[needs_copy] <- copy(out[needs_copy])
    out <- new_tidytable(out)
    out <- tidytable_restore(out, x)
  } else {
    out <- x
  }

  shallow(out)
}

# Creates a shallow copy
# Can add new columns or rename columns without modify-by-reference
shallow <- function(x) {
  x[TRUE]
}

