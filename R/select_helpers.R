#' Select helpers
#'
#' @description
#' Please note these functions are leftover from before `tidytable` used `tidyselect`.
#' You can use these or the normal `tidyselect` helpers.
#'
#' These functions allow you to select variables based on their names.
#'
#' * `any_of.()`: Select using a character vector
#' * `contains.()`: Contains a literal string or regex match
#' * `everything.()`: Matches all variables
#' * `starts_with.()`: Starts with a prefix
#' * `ends_with.()`: Ends with a suffix
#'
#' @param match A character vector. If length > 1, the union of the
#'   matches is taken.
#' @param ignore.case If `TRUE`, the default, ignores case when matching
#'   names.
#' @param vars A character vector of variable names. When called
#'   from inside selecting functions like [select.()] these are
#'   automatically set to the names of the table.
#' @param x An index vector of names or locations.
#' @param ... These dots are for future extensions and must be empty.
#'
#' @md
#' @export
#'
#' @examples
#' example_dt <- tidytable(
#'   x = 1,
#'   y = 2,
#'   double_x = 2,
#'   double_y = 4)
#'
#' example_dt %>%
#'   select.(starts_with.("x"))
#'
#' example_dt %>%
#'   select.(ends_with.("y"))
starts_with. <- tidyselect::starts_with

#' @export
#' @rdname starts_with.
dt_starts_with <- starts_with.

#' @export
#' @rdname starts_with.
contains. <- tidyselect::contains

#' @export
#' @rdname starts_with.
dt_contains <- contains.

#' @export
#' @rdname starts_with.
ends_with. <- tidyselect::ends_with

#' @export
#' @rdname starts_with.
dt_ends_with <- ends_with.

#' @export
#' @rdname starts_with.
everything. <- tidyselect::everything

#' @export
#' @rdname starts_with.
dt_everything <- everything.

#' @export
#' @rdname starts_with.
any_of. <- tidyselect::any_of

#' @export
#' @rdname starts_with.
dt_any_of <- any_of.
