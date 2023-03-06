#' Convert to a rowwise tidytable
#'
#' @description
#' Convert to a rowwise tidytable.
#'
#' @param .df A data.frame or data.table
#'
#' @export
#'
#' @examples
#' df <- tidytable(x = 1:3, y = 1:3 * 2, z = 1:3 * 3)
#'
#' # Compute the mean of x, y, z in each row
#' df %>%
#'   rowwise() %>%
#'   mutate(row_mean = mean(c(x, y, z)))
#'
#' # Use c_across() to more easily select many variables
#' df %>%
#'   rowwise() %>%
#'   mutate(row_mean = mean(c_across(x:z))) %>%
#'   ungroup()
rowwise <- function(.df) {
  .df <- .df_as_tidytable(.df)

  if (is_ungrouped(.df)) {
    set_class(.df, c("rowwise_tt", tidytable_class()))
  } else {
    out <- ungroup(.df)
    rowwise(out)
  }
}

#' @export
#' @keywords internal
#' @inherit rowwise
rowwise. <- function(.df) {
  deprecate_dot_fun()
  rowwise(.df)
}

