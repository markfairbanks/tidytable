#' Fill in missing values with previous or next value
#'
#' @description
#' Fills missing values in the selected columns using the next or previous entry. Can be done by group.
#'
#' Supports tidyselect
#'
#' @param .df A data.frame or data.table
#' @param ... A selection of columns. `tidyselect` compatible.
#' @param .direction Direction in which to fill missing values.
#' Currently "down" (the default), "up", "downup" (first down then up), or "updown" (first up and then down)
#' @param .by Columns to group by when filling should be done by group
#'
#' @export
#'
#' @examples
#' df <- data.table(
#'   a = c(1, NA, 3, 4, 5),
#'   b = c(NA, 2, NA, NA, 5),
#'   groups = c("a", "a", "a", "b", "b")
#' )
#'
#' df %>%
#'   fill(a, b)
#'
#' df %>%
#'   fill(a, b, .by = groups)
#'
#' df %>%
#'   fill(a, b, .direction = "downup", .by = groups)
fill <- function(.df, ...,
                 .direction = c("down", "up", "downup", "updown"),
                 .by = NULL) {
  fill.(.df, ..., .direction = .direction, .by = {{ .by }})
}

#' @export
#' @keywords internal
#' @inherit fill
fill. <- function(.df, ...,
                  .direction = c("down", "up", "downup", "updown"),
                  .by = NULL) {
  UseMethod("fill.")
}

#' @export
fill..tidytable <- function(.df, ...,
                            .direction = c("down", "up", "downup", "updown"),
                            .by = NULL) {
  .direction <- arg_match(.direction)

  dots <- enquos(...)

  mutate(.df, across(c(!!!dots), fill_na, .direction), .by = {{ .by }})
}

#' @export
fill..data.frame <- function(.df, ...,
                            .direction = c("down", "up", "downup", "updown"),
                            .by = NULL) {
  .df <- as_tidytable(.df)
  fill(.df, ..., .direction = .direction, .by = {{ .by }})
}

fill_na <- function(x, direction) {
  if (is.numeric(x)) {
    if (direction %in% c("down", "up")) {
      type <- switch(direction, "down" = "locf", "up" = "nocb")

      nafill(x, type = type)
    } else {
      if (direction == "downup") {
        type1 <- "locf"
        type2 <- "nocb"
      } else {
        type1 <- "nocb"
        type2 <- "locf"
      }

      nafill(nafill(x, type = type1), type = type2)
    }
  } else {
    vec_fill_missing(x, direction = direction)
  }
}

