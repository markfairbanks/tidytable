#' Select helpers
#'
#' @description
#' These functions allow you to select variables based on their names.
#'
#' * `dt_any_of()`: Select using a character vector
#' * `dt_contains()`: Contains a literal string
#' * `dt_everything()`: Matches all variables
#' * `dt_starts_with()`: Starts with a prefix
#' * `dt_ends_with()`: Ends with a suffix
#'
#' @param match The match for the helper function to use
#' @param x Character vector of columns to select
#'
#' @md
#' @export
#'
#' @examples
#' example_dt <- data.table::data.table(
#'   x = 1,
#'   y = 2,
#'   double_x = 2,
#'   double_y = 4)
#'
#' example_dt %>%
#'   dt_select(dt_starts_with("x"))
#'
#' example_dt %>%
#'   dt_select(dt_ends_with("y"))
dt_starts_with <- function(match) {
  .names <- names(caller_env())
  .names <- .names[!.names %in% predicate_names]

  seq_along(.names)[str_starts(.names, match)]
}

#' @export
#' @rdname dt_starts_with
dt_contains <- function(match) {
  .names <- names(caller_env())
  .names <- .names[!.names %in% predicate_names]

  seq_along(.names)[str_detect(.names, match)]
}

#' @export
#' @rdname dt_starts_with
dt_ends_with <- function(match) {
  .names <- names(caller_env())
  .names <- .names[!.names %in% predicate_names]

  seq_along(.names)[str_ends(.names, match)]
}

#' @export
#' @rdname dt_starts_with
dt_everything <- function() {
  .names <- names(caller_env())
  .names <- .names[!.names %in% predicate_names]

  seq_along(.names)
}

#' @export
#' @rdname dt_starts_with
dt_any_of <- function(x) {
  .names <- names(caller_env())
  .names <- .names[!.names %in% predicate_names]

  seq_along(.names)[.names %in% x]
}

predicate_names <- c("is.integer", "is.double", "is.numeric",
                     "is.character", "is.factor", "is.logical", "is.list")
