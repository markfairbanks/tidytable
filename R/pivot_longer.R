#' Pivot data from wide to long
#'
#'
#' @description
#' \code{dt_pivot_wider()} "widens" data, increasing the number of columns and
#' decreasing the number of rows. The inverse transformation is
#' \code{dt_pivot_longer()}. Syntax based on the \code{tidyr} equivalents.
#'
#' Supports enhanced selection
#'
#' @param .data The data table to pivot longer
#' @param cols Column selection. If empty, uses all columns. Can use -colname to unselect column(s)
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
#' dt_pivot_longer(example_dt,
#'                 cols = c(x, y),
#'                 names_to = "stuff",
#'                 values_to = "things")
#'
#' dt_pivot_longer(example_dt,
#'                 cols = -z,
#'                 names_to = "stuff",
#'                 values_to = "things")
#'
#' @export
dt_pivot_longer <- function(.data,
                            cols = NULL,
                            names_to = "name",
                            values_to = "value",
                            values_drop_na = FALSE,
                            ...) {

  if (!is.data.frame(.data)) stop("dt_ must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  names <- colnames(.data)
  cols <- enexpr(cols)

  if (is.null(cols)) {
    # All columns if cols = NULL
    cols <- names
  } else {
    cols <- vec_selector(.data, !!cols) %>%
      as.character()
  }

  if (length(cols) == 0) warning("No columns remaining after removing")

  id_vars <- names[!names %in% cols]

  melt(data = .data,
       id.vars = id_vars,
       measure.vars = cols,
       variable.name = names_to,
       value.name = values_to,
       ...,
       na.rm = values_drop_na,
       variable.factor = FALSE,
       value.factor = FALSE)
}
