.onLoad <- function(libname, pkgname) {
  library(data.table)
  library(rlang)
  library(magrittr)
  library(purrr)
  library(stringr)
  set_colon_equal_alias()
  invisible()
}
