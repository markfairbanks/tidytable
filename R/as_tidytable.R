#' Coerce an object to a data.table/tidytable
#'
#' @description
#' A tidytable object is simply a data.table with nice printing features.
#'
#' Note that all tidytable functions automatically convert data.frames & data.tables to tidytables in the background.
#' As such this function will rarely need to be used by the user.
#'
#' @param x An R object
#' @param .name_repair Treatment of duplicate names. See `?vctrs::vec_as_names` for options/details.
#' @param .keep_rownames Default is `FALSE`. If `TRUE`, adds the input object's names as a separate
#' column named `"rn"`. `.keep_rownames = "id"` names the column "id" instead.
#' @param ... Additional arguments to be passed to or from other methods.
#'
#' @export
#'
#' @examples
#' test_df <- data.frame(x = -2:2, y = c(rep("a", 3), rep("b", 2)))
#'
#' test_df %>%
#'   as_tidytable()
as_tidytable <- function(x, ...,
                         .name_repair = c("check_unique", "unique", "universal", "minimal"),
                         .keep_rownames = NULL) {
  UseMethod("as_tidytable")
}

#' @export
as_tidytable.tidytable <- function(x, ...,
                                   .name_repair = c("check_unique", "unique", "universal", "minimal")) {
  x
}

#' @export
as_tidytable.data.table <- function(x, ...,
                                    .name_repair = c("check_unique", "unique", "universal", "minimal")) {
  x <- add_tidytable_class(x)

  df_name_repair(x, .name_repair = .name_repair)
}

#' @export
as_tidytable.data.frame <- function(x, ...,
                                    .name_repair = c("check_unique", "unique", "universal", "minimal"),
                                    .keep_rownames = FALSE) {

  x <- as.data.table(x, keep.rownames = .keep_rownames)
  x <- add_tidytable_class(x)

  df_name_repair(x, .name_repair = .name_repair)
}

#' @export
as_tidytable.default <- function(x, ...) {
  add_tidytable_class(as.data.table(x))
}

add_tidytable_class <- function(x) {
  class(x) <- c("tidytable", "data.table", "data.frame")
  x
}
