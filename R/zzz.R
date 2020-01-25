.onLoad <- function(libname, pkgname) {
  library(data.table, warn.conflicts = FALSE, quietly = TRUE)
  library(magrittr, warn.conflicts = FALSE, quietly = TRUE)
  library(rlang, warn.conflicts = FALSE, quietly = TRUE)
  invisible()
}
