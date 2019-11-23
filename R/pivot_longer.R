#' Pivot data from wide to long
#'
#' `pivot_longer()` "lengthens" data, increasing the number of rows and
#' decreasing the number of columns. The inverse transformation is
#' [pivot_wider()]
#'
#' @param data
#' @param cols Column selection. If empty uses all columns.
#' @param names_to Name of the new "names" column. Must be a string.
#' @param values_to Name of the new "values" column. Must be a string.
#'
#' @return
#' @export
#'
#' @examples
#' example_df <- data.table(x = c(1,2,3), y = c(4,5,6), z = c("a", "b", "c"))
#' example_df %>%
#'   as_dt() %>%
#'   dt_pivot_longer(cols = c(x, y), names_to = "stuff", values_to = "things")
dt_pivot_longer <- function(data,
                            cols = NULL,
                            names_to = "name",
                            values_to = "value") {
  if (missing(cols)) {
    cols <- colnames(data)
  } else {
    cols <- enexpr(cols)
    cols <- characterize(cols)
  }

  id_vars <- colnames(data)[colnames(data) %notin% cols]

  data.table::melt(data = data,
                   id.vars = id_vars,
                   measure.vars = cols,
                   variable.name = names_to,
                   value.name = values_to,
                   # ...,
                   na.rm = FALSE,
                   variable.factor = FALSE,
                   value.factor = FALSE)
}
