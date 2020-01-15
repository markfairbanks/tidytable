#' Rename a selection of variables
#'
#' @description
#' These scoped variants of `rename()`` operate on a selection of variables
#'
#' @usage
#' dt_rename_all(.data, .fun, ...)
#' dt_rename_at(.data, .vars, .fun, ...)
#' dt_rename_if(.data, .predicate, .fun, ...)
#'
#'
#' @param .data A data.frame or data.table
#' @param .predicate Predicate to specify columns for `dt_rename_if()`
#' @param .vars `list()` of variables for `dt_rename_at()` to use
#' @param .fun Function to pass
#' @param ... Other arguments for the passed function
#'
#' @md
#' @return
#' @export
#'
#' @examples
#'
#' example_dt <- data.table(x = 1, y = 2, double_x = 2, double_y = 4)
#'
#' example_dt %>% dt_rename_all(str_replace, "x", "stuff")
#'
#' example_dt %>% dt_rename_at(list(x, double_x), str_replace, "x", "stuff")
#'
#' example_dt %>% dt_rename_if(is.double, function(x) str_replace(x, "x", "stuff"))
dt_rename_all <- function(.data, .fun, ...) {

  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  .cols <- colnames(.data)

  for (old_name in .cols) {
    new_name <- .fun(old_name, ...)
    setnames(.data, old_name, new_name)
  }
  .data
}

#' @export
#' @rdname dt_rename_all
dt_rename_at <- function(.data, .vars, .fun, ...) {

  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  .cols <- column_selector(.data, substitute(.vars))

  if (length(.cols) > 0) {
    for (old_name in .cols) {
      new_name <- .fun(old_name, ...)
      setnames(.data, old_name, new_name)
    }
    .data
  } else {
    .data
  }
}

#' @export
#' @rdname dt_rename_all
dt_rename_if <- function(.data, .predicate, .fun, ...) {

  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  .cols <- colnames(.data)[dt_map_lgl(.data, .predicate)]

  if (length(.cols) > 0) {
    for (old_name in .cols) {
      new_name <- .fun(old_name, ...)
      setnames(.data, old_name, new_name)
    }
    .data
  } else {
    .data
  }
}
