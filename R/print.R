# tidytable ---------------------------
#' @export
print.tidytable <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  y <- set_class(x, print_class("tidytable_print"))
  print(y, ..., n = n, width = width, n_extra = n_extra)
  invisible(x)
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

# grouped_tt ---------------------------
#' @export
print.grouped_tt <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  y <- set_class(x, print_class("grouped_tt_print"))
  print(y, ..., n = n, width = width, n_extra = n_extra)
  invisible(x)
}

#' @export
tbl_sum.grouped_tt_print <- function(x) {
  c("A grouped tidytable" = dim_desc(x))
}

#' @export
vec_ptype_abbr.grouped_tt <- function(x) {
  "grouped_tt"
}

# rowwise_tt ---------------------------
#' @export
print.rowwise_tt <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  y <- set_class(x, print_class("rowwise_tt_print"))
  print(y, ..., n = n, width = width, n_extra = n_extra)
  invisible(x)
}

#' @export
tbl_sum.rowwise_tt_print <- function(x) {
  c("A rowwise tidytable" = dim_desc(x))
}

#' @export
vec_ptype_abbr.rowwise_tt <- function(x) {
  "rowwise_tt"
}

print_class <- function(.class) {
  c("paged_df", .class, "tbl", "tidytable", "data.table", "data.frame")
}
