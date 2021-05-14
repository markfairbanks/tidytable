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
#' @md
#'
#' @examples
#' test_df <- data.table(
#'   x = c(1, 2, NA),
#'   y = c(NA, 1, 2)
#' )
#'
#' # Using replace_na.() inside mutate.()
#' test_df %>%
#'   mutate.(x = replace_na.(x, 5))
#'
#' # Using replace_na.() on a data frame
#' test_df %>%
#'   replace_na.(list(x = 5, y = 0))
replace_na. <- function(.x, replace = NA) {
  UseMethod("replace_na.")
}

#' @export
replace_na..default <- function(.x, replace = NA) {
  vec_assert(replace, size = 1)

  if (is.integer(.x) || is.double(.x)) {
    nafill(.x, "const", fill = replace)
  } else if (vec_is_list(.x)) {
    null_flag <- map_lgl.(.x, is.null)
    .x[null_flag] <- replace
    .x
  } else {
    replace <- vec_cast(replace, vec_ptype(.x))

    .x %|% replace
  }
}

#' @export
replace_na..tidytable <- function(.x, replace = list()) {
  stopifnot(vec_is_list(replace))

  if (length(replace) == 0) return(.x)

  replace_vars <- intersect(names(replace), names(.x))

  calls <- vector("list", length(replace_vars))
  names(calls) <- replace_vars
  for (i in seq_along(replace_vars)) {
    calls[[i]] <- call2(
      "replace_na.", sym(replace_vars[[i]]), replace[[i]],
      .ns = "tidytable"
    )
  }

  .x <- mutate.(.x, !!!calls)

  .x
}

#' @export
replace_na..data.frame <- function(.x, replace = list()) {
  .x <- as_tidytable(.x)
  replace_na.(.x, replace = replace)
}

