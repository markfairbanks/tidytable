.onLoad <- function(libname, pkgname) {
  requireNamespace("data.table")
  requireNamespace("magrittr")
  requireNamespace("rlang")
  setHook(packageEvent("knitr", "attach"),
          function(...) tidytable_first())
  invisible()
}

tidytable_first <- function(){
  detach("package:tidytable")
  library(tidytable, warn.conflicts = FALSE, quietly = TRUE)
}
