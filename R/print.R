# tidytable ---------------------------
#' @export
tbl_sum.tidytable <- function(x) {
  c("A tidytable" = dim_desc(x))
}

#' @export
vec_ptype_abbr.tidytable <- function(x, ..., prefix_named = FALSE, suffix_shape = TRUE) {
  "tidytable"
}

# grouped_tt ---------------------------
#' @export
tbl_sum.grouped_tt <- function(x) {
  groups <- glue_collapse(group_vars(x), sep = ", ")
  c("A tidytable" = dim_desc(x), "Groups" = groups)
}

#' @export
vec_ptype_abbr.grouped_tt <- function(x, ..., prefix_named = FALSE, suffix_shape = TRUE) {
  "grouped_tt"
}

# rowwise_tt ---------------------------
#' @export
tbl_sum.rowwise_tt <- function(x) {
  c("A rowwise tidytable" = dim_desc(x))
}

#' @export
vec_ptype_abbr.rowwise_tt <- function(x, ..., prefix_named = FALSE, suffix_shape = TRUE) {
  "rowwise_tt"
}
