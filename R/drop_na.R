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
#'
#' @examples
#' df <- data.table(
#'   x = c(1, 2, NA),
#'   y = c("a", NA, "b")
#' )
#'
#' df %>%
#'   drop_na()
#'
#' df %>%
#'   drop_na(x)
#'
#' df %>%
#'   drop_na(where(is.numeric))
drop_na <- function(.df, ...) {
  UseMethod("drop_na")
}

#' @export
drop_na.tidytable <- function(.df, ...) {
  if (missing(...)) {
    na.omit(.df)
  } else {
    cols <- tidyselect_locs(.df, ...)

    na.omit(.df, cols = cols)
  }
}

#' @export
drop_na.data.frame <- function(.df, ...) {
  .df <- as_tidytable(.df)
  drop_na(.df, ...)
}

