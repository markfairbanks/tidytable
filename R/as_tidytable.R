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
#' df <- data.frame(x = -2:2, y = c(rep("a", 3), rep("b", 2)))
#'
#' df %>%
#'   as_tidytable()
as_tidytable <- function(x, ...,
                         .name_repair = "unique",
                         .keep_rownames = NULL) {
  UseMethod("as_tidytable")
}

#' @export
as_tidytable.tidytable <- function(x, ...,
                                   .name_repair = "unique") {
  x
}

#' @export
as_tidytable.data.table <- function(x, ...,
                                    .name_repair = "unique") {
  x <- add_tidytable_class(x)

  df_name_repair(x, .name_repair = .name_repair)
}

#' @export
as_tidytable.data.frame <- function(x, ...,
                                    .name_repair = "unique",
                                    .keep_rownames = FALSE) {
  out <- fast_copy(new_tidytable(x))

  if (!is_false(.keep_rownames)) {
    if (is.character(.keep_rownames)) {
      col_name <- .keep_rownames
    } else {
      col_name <- "rn"
    }

    row_names <- new_tidytable(list2(!!col_name := rownames(x)))

    out <- vec_cbind(row_names, out)
  }

  df_name_repair(out, .name_repair = .name_repair)
}

#' @export
as_tidytable.list <- function(x, ...,
                              .name_repair = "unique") {
  x <- x[!map_lgl.(x, is.null)]
  x <- vec_recycle_common(!!!x)
  if (is.null(names(x))) {
    names(x) <- vec_rep("", length(x))
  }
  x <- new_tidytable(x)

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
