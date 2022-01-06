#' Add/modify columns by row
#'
#' @description
#' Allows you to mutate "by row". this is most useful when a vectorized function doesn't exist.
#'
#' @param .df A data.table or data.frame
#' @param ... Columns to add/modify
#'
#' @export
#'
#' @examples
#' df <- data.table(x = runif(6), y = runif(6), z = runif(6))
#'
#' # Compute the mean of x, y, z in each row
#' df %>%
#'   mutate_rowwise.(row_mean = mean(c(x, y, z)))
#'
#' # Use c_across.() to more easily select many variables
#' df %>%
#'   mutate_rowwise.(row_mean = mean(c_across.(x:z)))
mutate_rowwise. <- function(.df, ...) {
  UseMethod("mutate_rowwise.")
}

#' @export
mutate_rowwise..tidytable <- function(.df, ...) {
  dots <- enquos(...)
  if (length(dots) == 0) return(.df)

  .df <- mutate.(.df, .rowwise_id = .I)

  .df <- mutate.(.df, !!!dots, .by = .rowwise_id)

  mutate.(.df, .rowwise_id = NULL)
}

#' @export
mutate_rowwise..data.frame <- function(.df, ...) {
  .df <- as_tidytable(.df)
  mutate_rowwise.(.df, ...)
}

globalVariables(".rowwise_id")
