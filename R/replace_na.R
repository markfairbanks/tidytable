#' Replace missing values
#'
#' @description
#' Replace NAs with specified values
#'
#' @param .x A data.frame/data.table or a vector
#' @param replace If `.x` is a data frame, a `list()` of replacement values for specified columns.
#' If `.x` is a vector, a single replacement value.
#'
#' @export
#'
#' @examples
#' df <- data.table(
#'   x = c(1, 2, NA),
#'   y = c(NA, 1, 2)
#' )
#'
#' # Using replace_na() inside mutate()
#' df %>%
#'   mutate(x = replace_na(x, 5))
#'
#' # Using replace_na() on a data frame
#' df %>%
#'   replace_na(list(x = 5, y = 0))
replace_na <- function(.x, replace) {
  if (missing(replace)) return(.x)
  UseMethod("replace_na")
}

#' @export
replace_na.tidytable <- function(.x, replace) {
  stopifnot(obj_is_list(replace))

  keep <- names(replace) %in% names(.x)

  replace <- replace[keep]

  replace_vars <- names(replace)[keep]

  calls <- map2(syms(replace_vars), replace,
                ~ call2("replace_na", .x, .y, .ns = "tidytable"))

  names(calls) <- replace_vars

  mutate(.x, !!!calls)
}

#' @export
replace_na.data.frame <- function(.x, replace) {
  .x <- as_tidytable(.x)
  replace_na(.x, replace)
}

#' @export
replace_na.numeric <- function(.x, replace) {
  nafill(.x, "const", fill = replace)
}

#' @export
replace_na.default <- function(.x, replace) {
  missing <- vec_detect_missing(.x)
  vec_assign(.x, missing, replace)
}

#' @export
replace_na.list <- function(.x, replace) {
  missing <- vec_detect_missing(.x)
  .x[missing] <- replace
  .x
}
