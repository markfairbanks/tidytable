#' Pivot data from long to wide
#
#' @description
#'
#' `pivot_wider()` "widens" data, increasing the number of columns and
#' decreasing the number of rows. The inverse transformation is
#' [pivot_longer()].
#'
#' @param data
#' @param id_cols A set of columns that uniquely identifies each observation.
#'   Defaults to all columns in `data` except for the columns specified in
#'   `names_from` and `values_from`. Typically used when you have additional
#'   variables that is directly related.
#' @param names_from,values_from A pair of arguments describing which column
#'   (or columns) to get the name of the output column (`name_from`), and
#'   which column (or columns) to get the cell values from (`values_from`).
#' @param drop will cast by including all missing combinations.
#' c(FALSE, TRUE) will only include all missing combinations of formula LHS;
#' c(TRUE, FALSE) will only include all missing combinations of formula RHS. See Examples.
#'
#' @export
#'
#' @examples
#' library(data.table)
#'
#' example_dt <- data.table(z = rep(c("a", "b", "c"), 2),
#'                          stuff = c(rep("x", 3), rep("y", 3)),
#'                          things = 1:6)
#'
#' example_dt %>%
#'   dt_pivot_wider(names_from = stuff, values_from = things)
dt_pivot_wider <- function(data,
                           id_cols = NULL,
                           names_from = name,
                           names_sep = "_",
                           values_from = value,
                           drop = FALSE) {

  is.data.frame(data) || is.data.table(data) || stop("data must be a data.frame or data.table")

  if (!is.data.table(data)) {
    data <- as.data.table(data)
  }

  names_from <- characterize(substitute(names_from))
  values_from <- characterize(substitute(values_from))

  if (missing(id_cols)) {
    id_cols <- colnames(data)[!colnames(data) %in% c(names_from, values_from)]
  } else {
    id_cols <- characterize(substitute(id_cols))
  }

  if (length(id_cols) == 1) {
    dcast_form <- as.formula(paste(id_cols, paste(names_from, collapse = " + "), sep = " ~ "))
  } else {
    dcast_form <- as.formula(paste(paste(id_cols, collapse = " + "), paste(names_from, collapse=" + "), sep=" ~ "))
  }

  data.table::dcast(data,
                    formula = dcast_form,
                    value.var = values_from,
                    fun.aggregate = NULL,
                    sep = names_sep,
                    drop = drop)
}
