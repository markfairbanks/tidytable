#' Arrange by a selection of variables
#'
#' *Deprecated*
#'
#' Arrange all rows in either ascending or descending order by a selection of variables.
#'
#' @param .df A data.table or data.frame
#' @param .cols vector `c()` of unquoted column names. `tidyselect` compatible.
#' @param .fns Function to apply. If `desc` it arranges in descending order
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- tidytable(a = c("a", "b", "a"), b = 3:1)
#'
#' df %>%
#'   arrange_across.()
#'
#' df %>%
#'   arrange_across.(a, desc.)
#' }
arrange_across. <- function(.df, .cols = everything(), .fns = NULL) {
  UseMethod("arrange_across.")
}

#' @export
arrange_across..data.frame <- function(.df, .cols = everything(), .fns = NULL) {
  deprecate_old_across("arrange")

  .df <- as_tidytable(.df)
  .df <- copy(.df)

  .cols <- tidyselect_names(.df, {{ .cols }})
  if (length(.cols) == 0) return(.df)

  .fns <- enexpr(.fns)

  if (is_null(.fns)) {
    .order <- 1
  } else if (is_symbol(.fns, c("desc", "desc."))) {
    .order <- -1
  } else if (is_call(.fns, "~")) {
    .fns <- f_rhs(.fns)
    if (is_call(.fns, c("desc", "desc."))) {
      .order <- -1
    } else {
      abort(".fns must be either NULL, desc, or ~ desc(.x)")
    }
  } else {
    abort(".fns must be either NULL, desc, or ~ desc(.x)")
  }

  setorderv(.df, cols = .cols, order = .order)

  .df
}
