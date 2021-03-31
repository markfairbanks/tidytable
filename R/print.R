#' @export
print.tidytable <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  class(x) <- c("paged_df", "tidytable_print", "tbl", "tidytable", "data.table", "data.frame")
  print(x, ..., n = n, width = width, n_extra = n_extra)
}

#' @export
tbl_sum.tidytable <- function(x) {
  c("A tidytable" = dim_desc(x))
}

#' @export
tbl_sum.tidytable_print <- function(x) {
  c("A tidytable" = dim_desc(x))
}

#' @export
vec_ptype_abbr.tidytable <- function(x) {
  "tidytable"
}

#' @export
vec_ptype_abbr.tidytable_print <- function(x) {
  "tidytable"
}
