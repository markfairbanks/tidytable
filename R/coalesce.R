#' Coalesce missing values
#'
#' @description
#' Fill in missing values in a vector by pulling successively from other vectors.
#'
#' @param ... Input vectors. Supports dynamic dots.
#'
#' @export
#'
#' @examples
#' # Use a single value to replace all missing values
#' x <- sample(c(1:5, NA, NA, NA))
#' coalesce.(x, 0)
#'
#' # Or match together a complete vector from missing pieces
#' y <- c(1, 2, NA, NA, 5)
#' z <- c(NA, NA, 3, 4, 5)
#' coalesce.(y, z)
#'
#' # Supply lists with dynamic dots
#' vecs <- list(
#'   c(1, 2, NA, NA, 5),
#'   c(NA, NA, 3, 4, 5)
#' )
#' coalesce.(!!!vecs)
coalesce. <- function(...) {
  values <- list2(...)
  values <- vec_cast_common(!!!values)

  dt_expr <- call2_dt("fcoalesce", !!!values)
  eval_tidy(dt_expr)
}
