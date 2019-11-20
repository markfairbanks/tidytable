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
#'
#' @return
#' @export
dt_pivot_wider <- function(data,
                           id_cols = NULL,
                           names_from = name,
                           names_prefix = "",
                           names_sep = "_",
                           values_from = value) {

  if (length(as.character(substitute(names_from))) == 1) {
    names_from <- as.character(substitute(names_from))
  } else {
    names_from <- as.character(substitute(names_from)[-1])
  }

  if (length(as.character(substitute(values_from))) == 1) {
    values_from <- as.character(substitute(values_from))
  } else {
    values_from <- as.character(substitute(values_from)[-1])
  }

  if (missing(id_cols)) {
    id_cols <- colnames(data)[!colnames(data) %in% c(names_from, values_from)]
  } else if (length(as.character(substitute(id_cols))) == 1) {
    id_cols <- as.character(substitute(id_cols))
  } else {
    id_cols <- as.character(substitute(id_cols)[-1])
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
                    drop = FALSE)
}
