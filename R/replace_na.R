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

  if (is.data.frame(.x)) {
    .x <- .df_as_tidytable(.x)

    stopifnot(obj_is_list(replace))

    keep <- names(replace) %in% names(.x)

    replace <- replace[keep]

    replace_vars <- names(replace)[keep]

    calls <- map2(syms(replace_vars), replace,
                  ~ call2("replace_na", .x, .y, .ns = "tidytable"))

    names(calls) <- replace_vars

    out <- mutate(.x, !!!calls)
  } else {
    if (is.numeric(.x)) {
      out <- nafill(.x, "const", fill = replace)
    } else {
      out <- .x
      missing <- vec_detect_missing(out)
      out[missing] <- replace
    }
  }
  out
}

#' @export
#' @keywords internal
#' @inherit replace_na
replace_na. <- function(.x, replace) {
  deprecate_dot_fun()
  replace_na(.x, replace)
}


