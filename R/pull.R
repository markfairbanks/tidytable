#' Pull out a single variable
#'
#' @description
#' Pull a single variable from a data.table as a vector.
#'
#' @param .df A data.frame or data.table
#' @param var The column to pull from the data.table as:
#' * a variable name
#' * a positive integer giving the column position
#' * a negative integer giving the column position counting from the right
#' @param name Optional - specifies the column to be used as names for the vector.
#'
#' @export
#'
#' @examples
#' df <- data.table(
#'   x = 1:3,
#'   y = 1:3
#' )
#'
#' # Grab column by name
#' df %>%
#'   pull(y)
#'
#' # Grab column by position
#' df %>%
#'   pull(1)
#'
#' # Defaults to last column
#' df %>%
#'   pull()
pull <- function(.df, var = -1, name = NULL) {
  vec <- .pull(.df, {{ var }})

  name <- enquo(name)
  if (!quo_is_null(name)) {
    names(vec) <- .pull(.df, !!name)
  }

  vec
}

.pull <- function(.df, var) {
  vars <- as.list(seq_along(.df))

  names(vars) <- names(.df)

  var <- eval_tidy(enquo(var), vars)

  if (var < 0) {
    var <- length(vars) + var + 1
  }

  .df[[var]]
}
