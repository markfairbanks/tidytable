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
#' @param match The match for the helper function to use
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
  .names <- .names[!.names %in% c("is.integer", "is.double", "is.numeric",
                                  "is.character", "is.factor", "is.logical")]

  seq_along(.names)[str_starts(.names, match)]
}

#' @export
#' @rdname dt_starts_with
dt_contains <- function(match) {
  .names <- names(caller_env())
  .names <- .names[!.names %in% c("is.integer", "is.double", "is.numeric",
                                  "is.character", "is.factor", "is.logical")]

  seq_along(.names)[str_detect(.names, match)]
}

#' @export
#' @rdname dt_starts_with
dt_ends_with <- function(match) {
  .names <- names(caller_env())
  .names <- .names[!.names %in% c("is.integer", "is.double", "is.numeric",
                                  "is.character", "is.factor", "is.logical")]

  seq_along(.names)[str_ends(.names, match)]
}

#' @export
#' @rdname dt_starts_with
dt_everything <- function() {
  .names <- names(caller_env())
  .names <- .names[!.names %in% c("is.integer", "is.double", "is.numeric",
                                  "is.character", "is.factor", "is.logical")]

  seq_along(.names)
}
