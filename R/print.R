#' @export
print.tidytable <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  if (knitr_installed && knitr_loaded) {

    paged_print <- knitr::opts_chunk$get()$paged.print %||% TRUE
    knitting <- isTRUE(getOption('knitr.in.progress'))

    if (paged_print && !knitting)
      print(rmarkdown::paged_table(x))
    else
      print_mat(x, n = n, width = width, n_extra = n_extra)

  } else {
    print_mat(x, n = n, width = width, n_extra = n_extra)
  }
}

print_mat <- function(x, n = NULL, width = NULL, n_extra = NULL) {
  mat <- trunc_mat(x, n = n, width = width, n_extra = n_extra)
  mat$summary <- unname(mat$summary)
  print(mat)
}

knitr_installed <- rlang::is_installed("knitr")
knitr_loaded <- "knitr" %in% loadedNamespaces()

#' @export
vec_ptype_abbr.tidytable <- function(x) {
  "tidytable"
}
