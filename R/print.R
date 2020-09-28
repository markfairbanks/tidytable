#' @export
print.tidytable <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  class(x) <- c("paged_df", "tidytable_print", "tidytable", "data.table", "data.frame")
  print(x, ..., n = n, width = width, n_extra = n_extra)
}

#' @export
print.tidytable_print <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  mat <- trunc_mat(x, n = n, width = width, n_extra = n_extra)
  mat$summary <- unname(mat$summary)
  print(mat)
}

#' @title knit_print method for tidytables
#'
#' @description knit_print method for tidytables
#'
#' @keywords internal
#'
#' @export
knit_print.tidytable <- print.tidytable

#' @export
vec_ptype_abbr.tidytable <- function(x) {
  "tidytable"
}

#' @export
vec_ptype_abbr.tidytable_print <- function(x) {
  "tidytable"
}
