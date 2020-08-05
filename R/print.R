#' @export
print.tidytable <- function(x, ...) {
  if (knitr_installed) {
    if (knitr::opts_chunk$get()$paged.print %||% FALSE) {
      print(rmarkdown::paged_table(x))
    } else {
      mat <- trunc_mat(x)
      mat$summary <- unname(mat$summary)
      print(mat)
    }
  } else {
    mat <- trunc_mat(x)
    mat$summary <- unname(mat$summary)
    print(mat)
  }
}

knitr_installed <- rlang::is_installed("knitr")

#' @export
vec_ptype_abbr.tidytable <- function(x) {
  "tidytable"
}
