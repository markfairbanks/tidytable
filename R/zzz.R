.onLoad <- function(libname, pkgname) {
  requireNamespace("data.table")
  requireNamespace("magrittr")
  requireNamespace("rlang")
  options("datatable.print.class" = TRUE)
  options("datatable.print.trunc.cols" = TRUE)
  options("datatable.print.nrows" = 15)

  # Ensure that tidytable print settings are default
  setHook(packageEvent("data.table", "attach"), function(...) tidytable_first())
  invisible()
}

tidytable_first <- function() {
  detach("package:tidytable")
  library(tidytable, warn.conflicts = FALSE, quietly = TRUE)
}
