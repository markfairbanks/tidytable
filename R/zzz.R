.onLoad <- function(libname, pkgname) {
  library(data.table)
  library(rlang)
  library(magrittr)
  library(purrr)
  set_colon_equal_alias()
  invisible()
}
