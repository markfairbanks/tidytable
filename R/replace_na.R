#' Replace missing values
#'
#' @description
#' A shortcut to replace NAs inside of a `dt_mutate()` call.
#'
#' Note: This function *does not work* outside of `dt_mutate()` like `tidyr::replace_na()` does.
#'
#' @param .col A vector
#' @param replace A single value used for replacement
#'
#' @return
#' @export
#' @md
#'
#' @examples
#' example_dt <- data.table(x = c(1, 2, NA), y = c(NA, 1, 2))
#' example_dt %>%
#'   dt_mutate(x = dt_replace_na(x, 5))
dt_replace_na <- function(.col, replace) {

  nafill(.col, "const", fill = replace)

}
