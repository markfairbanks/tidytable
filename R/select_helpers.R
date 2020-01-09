#' Select helpers
#'
#' @description
#' These functions allow you to select variables based on their names.
#'
#' * `dt_starts_with()`: Starts with a prefix
#' * `dt_starts_with()`: Ends with a suffix
#' * `dt_contains()`: Contains a literal string
#' * `dt_everything()`: Matches all variables
#'
#' @param match
#'
#' @md
#' @return
#' @export
#'
#' @examples
dt_starts_with <- function(match) {
  .names <- names(parent.frame())

  seq_along(.names)[startsWith(.names, match)]
}

#' @export
#' @inherit dt_mutate
dt_contains <- function(match) {
  .names <- names(parent.frame())

  seq_along(.names)[grepl(match, .names)]
}

#' @export
#' @inherit dt_mutate
dt_ends_with <- function(match) {
  .names <- names(parent.frame())

  seq_along(.names)[endsWith(.names, match)]
}

#' @export
#' @inherit dt_mutate
dt_everything <- function() {
  .names <- names(parent.frame())

  seq_along(.names)
}
