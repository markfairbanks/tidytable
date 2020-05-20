#' Replace missing values
#'
#' @description
#' A shortcut to replace NAs inside of a `mutate.()` call.
#'
#' Note: This function *does not work* outside of `mutate.()` like `tidyr::replace_na()` does.
#'
#' @param .col A vector
#' @param replace A single value used for replacement
#'
#' @export
#' @md
#'
#' @examples
#' test_df <- data.table(
#'   x = c(1, 2, NA),
#'   y = c(NA, 1, 2))
#'
#' test_df %>%
#'   mutate.(x = replace_na.(x, 5))
replace_na. <- function(.col, replace) {

  if (class(replace) %in% c("integer", "double", "numeric")) {
    nafill(.col, "const", fill = replace)
  } else {
    ifelse.(is.na(.col), replace, .col)
  }
}

#' @export
#' @rdname replace_na.
dt_replace_na <- replace_na.
