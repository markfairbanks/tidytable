#' Pivot data from long to wide
#'
#' @description
#' \code{pivot_wider.()} "widens" data, increasing the number of columns and
#' decreasing the number of rows. The inverse transformation is
#' \code{pivot_longer.()}. Syntax based on the \code{tidyr} equivalents.
#'
#' @param .df the data table to widen
#' @param id_cols A set of columns that uniquely identifies each observation.
#' Defaults to all columns in the data table except for the columns specified in \code{names_from} and \code{values_from}.
#' Typically used when you have additional variables that is directly related.
#' `tidyselect` compatible.
#' @param names_from A pair of arguments describing which column (or columns) to get the name of the output column (\code{name_from}),
#' and which column (or columns) to get the cell values from (\code{values_from}).
#' `tidyselect` compatible.
#' @param values_from A pair of arguments describing which column (or columns) to get the name of the output column (\code{name_from}),
#' and which column (or columns) to get the cell values from (\code{values_from}).
#' `tidyselect` compatible.
#' @param names_sep the separator between the names of the columns
#' @param values_fn Should the data be aggregated before casting? If the formula doesn't identify a single observation for each cell, then aggregation defaults to length with a message.
#'
#' @examples
#' test_df <- data.table(
#'   z = rep(c("a", "b", "c"), 2),
#'   stuff = c(rep("x", 3), rep("y", 3)),
#'   things = 1:6)
#'
#' test_df %>%
#'   pivot_wider.(names_from = stuff, values_from = things)
#'
#' test_df %>%
#'   pivot_wider.(names_from = stuff, values_from = things, id_cols = z)
#'
#' @export
#' @md
pivot_wider. <- function(.df,
                         names_from = name,
                         values_from = value,
                         id_cols = NULL,
                         names_sep = "_",
                         values_fn = NULL) {
  UseMethod("pivot_wider.")
}

#' @export
pivot_wider..data.frame <- function(.df,
                                  names_from = name,
                                  values_from = value,
                                  id_cols = NULL,
                                  names_sep = "_",
                                  values_fn = NULL) {

  .df <- as_tidytable(.df)

  id_cols <- enquo(id_cols)
  values_fn <- enquo(values_fn)

  names_from <- select_vec_chr(.df, {{ names_from }})
  values_from <- select_vec_chr(.df, {{ values_from }})

  if (quo_is_null(id_cols)) {
    data_names <- names(.df)
    id_cols <- data_names[!data_names %in% c(names_from, values_from)]
  } else {
    id_cols <- select_vec_chr(.df, !!id_cols)
  }

  if (length(id_cols) == 0) {
    dcast_form <- as.formula(paste("...",
                                   paste(names_from, collapse = " + "),
                                   sep = " ~ "))
  } else {
    dcast_form <- as.formula(paste(paste(id_cols, collapse = " + "),
                                   paste(names_from, collapse = " + "),
                                   sep = " ~ "))
  }

  .df <- as_tidytable(
    eval_quo(
      dcast.data.table(.df,
                       formula = dcast_form,
                       value.var = values_from,
                       fun.aggregate = !!values_fn,
                       sep = names_sep,
                       drop = TRUE)
    )
  )

  if (length(id_cols) == 0) .df[, . := NULL]

  .df
}

#' @export
#' @rdname dt_verb
#' @inheritParams pivot_wider.
dt_pivot_wider <- function(.df,
                           names_from = name,
                           values_from = value,
                           id_cols = NULL,
                           names_sep = "_",
                           values_fn = NULL) {
  deprecate_stop("0.5.2", "tidytable::dt_pivot_wider()", "pivot_wider.()")

  pivot_wider.(.df,
               names_from = {{ names_from }},
               values_from = {{ values_from }},
               id_cols = id_cols,
               names_sep = names_sep,
               values_fn = {{ values_fn }})
}

