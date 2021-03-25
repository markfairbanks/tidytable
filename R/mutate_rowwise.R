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
#' test_df <- data.table(x = runif(6), y = runif(6), z = runif(6))
#'
#' # Compute the mean of x, y, z in each row
#' test_df %>%
#'   mutate_rowwise.(row_mean = mean(c(x, y, z)))
#'
#' # Use c_across.() to more easily select many variables
#' test_df %>%
#'   mutate_rowwise.(row_mean = mean(c_across.(x:z)))
mutate_rowwise. <- function(.df, ...) {
  UseMethod("mutate_rowwise.")
}

#' @export
mutate_rowwise..data.frame <- function(.df, ...) {
  .df <- as_tidytable(.df)

  dots <- enquos(...)
  if (length(dots) == 0) return(.df)

  # Need to prep expressions before .rowwise_id is added
  # Otherwise c_across.(cols = everything()) will grab .rowwise_id
  dots <- prep_exprs(dots, .df)

  .df <- mutate.(.df, .rowwise_id = .I)

  .df <- mutate.(.df, !!!dots, .by = .rowwise_id)

  mutate.(.df, .rowwise_id = NULL)
}

globalVariables(".rowwise_id")
