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
  replace_na.(.x, replace)
}

#' @export
#' @keywords internal
#' @inherit replace_na
replace_na. <- function(.x, replace) {
  UseMethod("replace_na.")
}

#' @export
replace_na..default <- function(.x, replace) {
  if (missing(replace)) {
    return(.x)
  }

  vec_assert(replace, size = 1)

  if (is.integer(.x) || is.double(.x)) {
    nafill(.x, "const", fill = replace)
  } else if (vec_is_list(.x)) {
    null_bool <- map_lgl(.x, is.null)
    .x[null_bool] <- replace
    .x
  } else {
    missing <- vec_equal_na(.x)
    vec_assign(.x, missing, replace)
  }
}

#' @export
replace_na..tidytable <- function(.x, replace) {
  if (missing(replace)) {
    return(.x)
  }

  stopifnot(vec_is_list(replace))

  keep_bool <- names(replace) %in% names(.x)

  replace <- replace[keep_bool]

  replace_vars <- names(replace)[keep_bool]

  calls <- map2(syms(replace_vars), replace,
                ~ call2("replace_na", .x, .y, .ns = "tidytable"))

  names(calls) <- replace_vars

  .x <- mutate(.x, !!!calls)

  .x
}

#' @export
replace_na..data.frame <- function(.x, replace) {
  .x <- as_tidytable(.x)
  replace_na(.x, replace = replace)
}


