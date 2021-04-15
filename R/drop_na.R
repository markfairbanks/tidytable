#' Drop rows containing missing values
#'
#' @description
#' Drop rows containing missing values
#'
#' @param .df A data.frame or data.table
#' @param ... Optional: A selection of columns. If empty, all variables are selected.
#' `tidyselect` compatible.
#'
#' @export
#' @md
#'
#' @examples
#' df <- data.table(
#'   x = c(1,2,NA),
#'   y = c("a",NA,"b")
#' )
#'
#' df %>%
#'   drop_na.()
#'
#' df %>%
#'   drop_na.(x)
#'
#' df %>%
#'   drop_na.(where(is.numeric))
drop_na. <- function(.df, ...) {
  UseMethod("drop_na.")
}

#' @export
drop_na..data.frame <- function(.df, ...) {
  .df <- as_tidytable(.df)

  dots <- enquos(...)

  if (length(dots) == 0) {
    na.omit(.df)
  } else {
    drop_cols <- select_dots_idx(.df, ...)

    na.omit(.df, cols = drop_cols)
  }
}
