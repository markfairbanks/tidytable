#' Recode values
#'
#' @description
#' _superseded_
#'
#' `recode()` has been superseded by `case_match()`.
#'
#' Replace old values of a vector with new values.
#'
#' @param .x A vector
#' @param ... A series of `old = new` pairs specifying the new values
#' @param .default The default value if all conditions evaluate to `FALSE`
#' @param .missing What missing values should be replaced with
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' char_vec <- c("a", "b", "c")
#' recode(char_vec, a = "Apple", b = "Banana")
#'
#' num_vec <- 1:3
#' recode(num_vec, `1` = 10, `2` = 25, .default = 100)
recode <- function(.x, ..., .default = NULL, .missing = NULL) {
  if (!is_simple_vector(.x)) {
    abort(".x must be a vector")
  }

  dots <- list2(...)

  if (length(dots) == 0) {
    return(.x)
  }

  old <- names(dots)
  if (is.null(old)) {
    abort("old values must be provided")
  }
  if (is.numeric(.x)) {
    old <- as.numeric(old)
  }

  new <- unname(dots)

  if (is.null(.default)) {
    .default <- .x
  }

  dots <- map2(old, new, ~ expr(!!.x ~ !!.y))

  out <- case_match(.x, !!!dots, .default = .default)

  if (!is.null(.missing)) {
    out[vec_detect_missing(.x)] <- .missing
  }

  if (is.factor(.x)) {
    out <- factor(out)
  }

  out
}
