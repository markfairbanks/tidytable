#' Pivot data from long to wide
#'
#' @description
#' \code{dt_pivot_wider()} "widens" data, increasing the number of columns and
#' decreasing the number of rows. The inverse transformation is
#' \code{dt_pivot_longer()}. Syntax based on the \code{tidyr} equivalents.
#'
#' Supports enhanced selection
#'
#' @param .data the data table to widen
#' @param id_cols A set of columns that uniquely identifies each observation. Defaults to all columns in the data table except for the columns specified in \code{names_from} and \code{values_from}. Typically used when you have additional variables that is directly related.
#' @param names_from A pair of arguments describing which column (or columns) to get the name of the output column (\code{name_from}), and which column (or columns) to get the cell values from (\code{values_from}).
#' @param names_sep the separator between the names of the columns
#' @param values_from A pair of arguments describing which column (or columns) to get the name of the output column (\code{name_from}), and which column (or columns) to get the cell values from (\code{values_from}).
#' @param drop When \code{FALSE}, will cast by including all missing combinations. When \code{TRUE}, it is drop missing combination. \code{c(FALSE, TRUE)} will only include all missing combinations of formula LHS; \code{c(TRUE, FALSE)} will only include all missing combinations of formula RHS.
#'
#' @examples
#'
#' library(data.table)
#'
#' example_dt <- data.table::data.table(
#'   z = rep(c("a", "b", "c"), 2),
#'   stuff = c(rep("x", 3), rep("y", 3)),
#'   things = 1:6)
#'
#' dt_pivot_wider(example_dt, names_from = stuff, values_from = things)
#' dt_pivot_wider(example_dt, names_from = stuff, values_from = things, id_cols = z)
#'
#' @export
dt_pivot_wider <- function(.data,
                           id_cols = NULL,
                           names_from,
                           names_sep = "_",
                           values_from,
                           drop = TRUE) {

  if (!is.data.frame(.data)) stop("dt_ must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  id_cols <- enexpr(id_cols)
  names_from <- enexpr(names_from)
  values_from <- enexpr(values_from)

  names_from <- vec_selector(.data, !!names_from) %>%
    as.character()
  values_from <- vec_selector(.data, !!values_from) %>%
    as.character()

  if (is.null(id_cols)) {
    id_cols <- colnames(.data)[!colnames(.data) %in% c(names_from, values_from)]
  } else {
    id_cols <- vec_selector(.data, !!id_cols) %>%
      as.character()
  }

  if (length(id_cols) == 0) {
    dcast_form <- as.formula(paste("...",
                                   paste(names_from, collapse = " + "),
                                   sep = " ~ "))
  } else {
    dcast_form <- as.formula(paste(paste(id_cols, collapse = " + "),
                                   paste(names_from, collapse=" + "),
                                   sep=" ~ "))
  }

  if (length(id_cols) == 0) {
    dcast.data.table(
      .data,
      formula = dcast_form,
      value.var = values_from,
      fun.aggregate = NULL,
      sep = names_sep,
      drop = drop)[, . := NULL][]
  } else {
    dcast.data.table(
      .data,
      formula = dcast_form,
      value.var = values_from,
      fun.aggregate = NULL,
      sep = names_sep,
      drop = drop)
  }
}
