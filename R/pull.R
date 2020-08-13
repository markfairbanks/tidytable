#' Pull out a single variable
#'
#' @description
#' Pull a single variable from a data.table as a vector.
#'
#' @param .df A data.frame or data.table
#' @param var The column to pull from the data.table. If NULL, pulls the last column.
#'
#' @export
#'
#' @examples
#' test_df <- data.table(
#'   x = c(1,2,3),
#'   y = c(4,5,6))
#'
#' test_df %>%
#'   pull.(y)
pull. <- function(.df, var = NULL) {
  UseMethod("pull.")
}

#' @export
pull..data.frame <- function(.df, var = NULL) {

  var<-enquo(var)
  var <- quo_name(var)

  if (var=="NULL") return(.df[[ncol(.df)]])
  
  .df[[if (suppressWarnings(!is.na(as.numeric(var)))) as.numeric(var) else var ]]
}

#' @export
#' @rdname dt_verb
#' @inheritParams pull.
dt_pull <- function(.df, var = NULL) {
  deprecate_soft("0.5.2", "tidytable::dt_pull()", "pull.()")

  pull.(.df, var = {{ var }})
}
