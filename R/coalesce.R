#' Coalesce missing values
#'
#' @description
#' Fill in missing values in a vector by pulling successively from other vectors.
#'
#' @param ... Input vectors. Supports dynamic dots.
#' @param .ptype Optional ptype to override output type
#' @param .size Optional size to override output size
#'
#' @export
#'
#' @examples
#' # Use a single value to replace all missing values
#' x <- c(1:3, NA, NA)
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
coalesce. <- function(..., .ptype = NULL, .size = NULL) {
  args <- list2(...)
  args <- vec_cast_common(!!!args, .to = .ptype)

  out <- do.call(fcoalesce, args)

  if (!is.null(.size)) {
    out <- vec_recycle(out, .size)
  }

  out
}
