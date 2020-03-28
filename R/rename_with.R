#' Rename multiple columns
#'
#' @description
#' Rename multiple columns with the same transformation
#'
#' @param .data A data.table or data.frame
#' @param .fn Function to transform the names with.
#' @param .cols Columns to rename. Defaults to all columns.
#' @param ... Other parameters to pass to the function
#'
#' @export
#'
#' @examples
#' example_dt <- data.table::data.table(
#'   x = 1,
#'   y = 2,
#'   double_x = 2,
#'   double_y = 4)
#'
#' example_dt %>%
#'   rename_with.(~ sub("x", "stuff", .x))
#'
#' example_dt %>%
#'   rename_with.(~ sub("x", "stuff", .x), .cols = c(x, double_x))
rename_with. <- function(.data, .fn, .cols = everything.(), ...) {
  UseMethod("rename_with.")
}

#' @export
rename_with..tidytable <- function(.data, .fn, .cols = everything.(), ...) {

  .cols <- enexpr(.cols)
  .cols <- as.character(vec_selector(.data, !!.cols))

  .data <- shallow(.data)

  .fn <- as_function(.fn)

  if (length(.cols) > 0) {

    new_names <- .fn(.cols, ...)
    setnames(.data, .cols, new_names)

    .data
  } else {
    .data
  }
}

#' @export
rename_with..data.frame <- function(.data, .fn, .cols = everything.(), ...) {
  .data <- as_tidytable(.data)
  .cols <- enexpr(.cols)

  rename_with.(.data, .fn = .fn, .cols = !!.cols, ...)
}

#' @export
#' @rdname rename_with.
dt_rename_with <- rename_with.
