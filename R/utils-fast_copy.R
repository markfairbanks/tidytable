# Copy columns that are being overwritten
# If columns are all new uses shallow copy
# Faster than running `copy()` on an entire dataset
fast_copy <- function(x, new_cols = character()) {
  if (length(new_cols) == 0) return(shallow(x))

  x_names <- copy(names(x))
  needs_copy <- new_cols %chin% x_names
  if (any(needs_copy)) {
    copy_cols <- new_cols[needs_copy]
  } else {
    copy_cols <- character()
  }

  if (length(copy_cols) == 0) {
    out <- shallow(x)
  } else {
    out <- vector("list", length(x_names))
    setattr(out, "names", x_names)
    for (col in x_names) {
      if (col %chin% copy_cols) {
        out[[col]] <- copy(x[[col]])
      } else {
        out[[col]] <- x[[col]]
      }
    }
    setDT(out)
    class <- copy(class(x))
    setattr(out, "class", class)
  }
  out[]
}

# Creates a shallow copy
# Can add new columns or rename columns without modify-by-reference
shallow <- function(x) {
  x[TRUE]
}

# deprecated shallow() ------------------------------------------------
# shallow <- function(x, cols = names(x), reset_class = FALSE) {
#   stopifnot(is.data.table(x), all(cols %in% names(x)))
#   ans <- vector("list", length(cols))
#   setattr(ans, 'names', copy(cols))
#   for (col in cols) {
#     ans[[col]] = x[[col]]
#   }
#   setDT(ans)
#   class  <- if (!reset_class) copy(class(x)) else c("data.table", "data.frame")
#   setattr(ans, 'class', class)
#   ans[]
# }
