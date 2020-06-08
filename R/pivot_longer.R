#' Pivot data from wide to long
#'
#'
#' @description
#' \code{pivot_wider.()} "widens" data, increasing the number of columns and
#' decreasing the number of rows. The inverse transformation is
#' \code{pivot_longer.()}. Syntax based on the \code{tidyr} equivalents.
#'
#' @param .df The data table to pivot longer
#' @param cols Vector of bare column names. Can add/drop columns. `tidyselect` compatible.
#' @param names_to Name of the new "names" column. Must be a string.
#' @param values_to Name of the new "values" column. Must be a string.
#' @param values_drop_na If TRUE, rows will be dropped that contain NAs.
#' @param ... Additional arguments to pass to `melt.df.table()`
#'
#' @export
#' @md
#'
#' @examples
#' test_df <- data.table(
#'   x = c(1,2,3),
#'   y = c(4,5,6),
#'   z = c("a", "b", "c"))
#'
#' test_df %>%
#'   pivot_longer.(c(x, y))
#'
#' test_df %>%
#'   pivot_longer.(cols = -z, names_to = "stuff", values_to = "things")
pivot_longer. <- function(.df,
                          cols = everything(),
                          names_to = "name",
                          values_to = "value",
                          values_drop_na = FALSE,
                          ...) {
  UseMethod("pivot_longer.")
}

#' @export
pivot_longer..data.frame <- function(.df,
                                     cols = everything(),
                                     names_to = "name",
                                     values_to = "value",
                                     values_drop_na = FALSE,
                                     ...) {

  .df <- as_tidytable(.df)

  names <- names(.df)

  cols <- select_vec_chr(.df, {{ cols }})

  if (length(cols) == 0) warning("No columns remaining after removing")

  id_vars <- names[!names %in% cols]

  as_tidytable(
    melt(data = .df,
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
#' @rdname pivot_longer.
dt_pivot_longer <- function(.df,
                            cols = everything(),
                            names_to = "name",
                            values_to = "value",
                            values_drop_na = FALSE,
                            ...) {
  deprecate_soft("0.5.2", "tidytable::dt_pivot_longer()", "pivot_longer.()")

  pivot_longer.(.df, cols = {{ cols }},
                names_to = names_to,
                values_to = values_to,
                values_drop_na = values_drop_na)
}
