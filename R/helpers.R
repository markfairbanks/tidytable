#' Helpers
#'
#' Helper functions to capture ..., list(), or c() inputs from the user.
#'
#' @param ... Dots
#'
#' @export
enlist_dots <- function(...) substitute(...())

#' @export
column_selector <- function(.data, select_vars) {

  data_names <- colnames(.data)

  data_vars <- setNames(as.list(seq_along(.data)), data_names)

  select_index <- unlist(eval(select_vars, data_vars))
  drop_index <- unique(abs(select_index[select_index < 0]))

  select_index <- setdiff(abs(unique(select_index)), drop_index)

  select_vars <- data_names[select_index]

  select_vars
}

#' @export
characterize <- function(vec_list_expr) {
  vle_length <- length(vec_list_expr)
  if (vle_length == 1) {
    as.character(vec_list_expr)
  } else if (as.character(vec_list_expr)[1] == "-"){
    as.character(vec_list_expr)
  } else {
    as.character(vec_list_expr)[-1]
  }
}
