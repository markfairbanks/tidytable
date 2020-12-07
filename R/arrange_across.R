#' Arrange by a selection of variables
#'
#' Arrange all rows in either ascending or descending order by a selection of variables.
#'
#' @param .df A data.table or data.frame
#' @param .cols vector `c()` of unquoted column names. `tidyselect` compatible.
#' @param .fns Function to apply. If `desc.` it arranges in descending order
#'
#' @export
#'
#' @examples
#' test_df <- tidytable(a = c("a", "b", "a"), b = 3:1)
#'
#' test_df %>%
#'   arrange_across.()
#'
#' test_df %>%
#'   arrange_across.(a, desc.)
arrange_across. <- function(.df, .cols = everything(), .fns) {
  UseMethod("arrange_across.")
}

#' @export
arrange_across..data.frame <- function(.df, .cols = everything(), .fns) {
  .df <- as_tidytable(.df)

  .cols <- select_vec_chr(.df, {{ .cols }})

  if (length(.cols) == 0) return(.df)

  if (missing(.fns)) {
    .order <- 1
  } else if (quo_text(enexpr(.fns)) %in% c("desc", "desc.")){
    .order <- -1
  } else {
    abort(".fns must be either missing or desc.")
  }

  .df <- copy(.df)

  setorderv(.df, cols = .cols, order = .order)

  .df
}
