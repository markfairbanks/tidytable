#' Pivot data from wide to long
#'
#'
#' @description
#' \code{pivot_wider.()} "widens" data, increasing the number of columns and
#' decreasing the number of rows. The inverse transformation is
#' \code{pivot_longer.()}. Syntax based on the \code{tidyr} equivalents.
#'
#' Supports enhanced selection
#'
#' @param .data The data table to pivot longer
#' @param cols Vector of bare column names. Can add/drop columns.
#' @param names_to Name of the new "names" column. Must be a string.
#' @param values_to Name of the new "values" column. Must be a string.
#' @param values_drop_na If TRUE, rows will be dropped that contain NAs.
#' @param ... Additional arguments to pass to `melt.data.table()`
#'
#'
#' @examples
#' example_dt <- data.table::data.table(
#'   x = c(1,2,3),
#'   y = c(4,5,6),
#'   z = c("a", "b", "c"))
#'
#' example_dt %>%
#'   pivot_longer.(c(x, y))
#'
#' example_dt %>%
#'   pivot_longer.(cols = -z, names_to = "stuff", values_to = "things")
#'
#' @export
pivot_longer. <- function(.data,
                          cols = everything.(),
                          names_to = "name",
                          values_to = "value",
                          values_drop_na = FALSE,
                          ...) {
  UseMethod("pivot_longer.")
}

#' @export
pivot_longer..tidytable <- function(.data,
                                    cols = everything.(),
                                    names_to = "name",
                                    values_to = "value",
                                    values_drop_na = FALSE,
                                    ...) {

  names <- colnames(.data)
  cols <- enexpr(cols)

  cols <- as.character(vec_selector(.data, !!cols))

  if (length(cols) == 0) warning("No columns remaining after removing")

  id_vars <- names[!names %in% cols]

  as_tidytable(
    melt(data = .data,
       id.vars = id_vars,
       measure.vars = cols,
       variable.name = names_to,
       value.name = values_to,
       ...,
       na.rm = values_drop_na,
       variable.factor = FALSE,
       value.factor = FALSE))
}

#' @export
pivot_longer..data.frame <- function(.data,
                                       cols = everything.(),
                                       names_to = "name",
                                       values_to = "value",
                                       values_drop_na = FALSE,
                                       ...) {
  .data <- as_tidytable(.data)
  cols <- enexpr(cols)

  .data <- pivot_longer.(
    .data, !!cols,
    names_to = names_to, values_to = values_to,
    values_drop_na = values_drop_na, ...)

  .data
}

#' @export
#' @rdname pivot_longer.
dt_pivot_longer <- pivot_longer.
