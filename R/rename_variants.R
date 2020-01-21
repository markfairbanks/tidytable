#' Rename a selection of variables
#'
#' @description
#' These scoped variants of `rename()` operate on a selection of variables
#'
#' Supports enhanced selection
#' @usage
#' dt_rename_all(.data, .fun, ...)
#'
#' dt_rename_across(.data, .predicate, .fun, ...)
#'
#'
#' @param .data A data.frame or data.table
#' @param .cols vector `c()` of bare column names for `dt_rename_at()` to use
#' @param .fun Function to pass
#' @param ... Other arguments for the passed function
#'
#' @md
#' @return
#' @export
#'
#' @examples
#'
#' example_dt <- data.table(x = 1,
#'                          y = 2,
#'                          double_x = 2,
#'                          double_y = 4)
#'
#' example_dt %>% dt_rename_all(str_replace, "x", "stuff")
#'
#' example_dt %>%
#'   dt_rename_across(c(x, double_x), str_replace, "x", "stuff")
#'
#' example_dt %>%
#'   dt_rename_across(is.double, function(x) str_replace(x, "x", "stuff"))
dt_rename_all <- function(.data, .fun, ...) {

  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  .cols <- colnames(.data)
  .fun <- anon_x(.fun)

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

  .vars <- enexpr(.vars)
  .vars <- vec_selector(.data, !!.vars) %>%
    as.character()

  .fun <- anon_x(.fun)

  if (length(.vars) > 0) {
    for (old_name in .vars) {
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
dt_rename_across <- function(.data, .cols, .fun, ...) {

  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  .vars <- enexpr(.cols)
  .vars <- vec_selector(.data, !!.vars) %>%
    as.character()

  .fun <- anon_x(.fun)

  if (length(.vars) > 0) {
    for (old_name in .vars) {
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

  .fun <- anon_x(.fun)

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
