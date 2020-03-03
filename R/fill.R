#' Fill in missing values with previous or next value
#'
#' @description
#' Fills missing values in the selected columns using the next or previous entry. Can be done by group.
#'
#' @param .data A data.frame or data.table
#' @param ... A selection of bare columns
#' @param .direction Direction in which to fill missing values. Currently "down" (the default), "up", "downup" (first down then up), or "updown" (first up and then down)
#' @param by Whether the filling should be done by group. Passed in a `list()`
#' @export
#' @md
#'
#' @examples
#' test_df <- data.table::data.table(
#'   x = c(NA, NA, NA, 4:10),
#'   y = c(1:6, NA, 8, NA, 10),
#'   z = c(rep("a", 8), rep("b", 2)))
#'
#' test_df %>%
#'   dt_fill(x, y, by = z)
#'
#' test_df %>%
#'   dt_fill(x, y, by = z, .direction = "downup")
dt_fill <- function(.data, ..., .direction = c("down", "up", "downup", "updown"), by = NULL) {
  UseMethod("dt_fill")
}

#' @export
dt_fill.tidytable <- function(.data, ..., .direction = c("down", "up", "downup", "updown"), by = NULL) {
  by <- enexpr(by)

  if (length(.direction) > 1) .direction <- "down"

  if (.direction == "down") {
    filldown(.data, ..., by = !!by)
  } else if (.direction == "up") {
    fillup(.data, ..., by = !!by)
  } else if (.direction == "downup") {
    .data %>%
      filldown(..., by = !!by) %>%
      fillup(..., by = !!by)
  } else {
    .data %>%
      fillup(..., by = !!by) %>%
      filldown(..., by = !!by)
  }
}

#' @export
dt_fill.data.frame <- function(.data, ..., .direction = c("down", "up", "downup", "updown"), by = NULL) {
  .data <- as_tidytable(.data)
  by <- enexpr(by)

  dt_fill(.data, ..., .direction = .direction, by = !!by)

}

filldown <- function(.data, ..., by = NULL) {

  dots <- dots_selector(.data, ...)
  by <- enexpr(by)

  for (dot in dots) {
    dot_type <- eval_tidy(expr(class('$'(.data, !!dot))))

    if (dot_type %in% c("integer", "double", "numeric")) {
      .data <- .data %>%
        dt_mutate(!!dot := nafill(!!dot, type = "locf"), by = !!by)
    } else if (dot_type %in% c("character", "logical", "factor")) {
      .data <- eval_tidy(expr(
        .data %>%
          dt_mutate(na_index = 1:.N, by = !!by) %>%
          dt_mutate(na_index = fifelse(is.na(!!dot), NA_integer_, na_index)) %>%
          dt_mutate(na_index = nafill(na_index, type = "locf"), by = !!by) %>%
          dt(, !!dot := .SD[, !!dot][na_index], by = !!by) %>%
          dt(, na_index := NULL) %>%
          dt()
      ))
    }
  }
  .data
}

fillup <- function(.data, ..., by = NULL) {

  dots <- dots_selector(.data, ...)
  by <- enexpr(by)

  for (dot in dots) {
    dot_type <- eval_tidy(expr(class('$'(.data, !!dot))))

    if (dot_type %in% c("integer", "double", "numeric")) {
      .data <- .data %>%
        dt_mutate(!!dot := nafill(!!dot, type = "nocb"), by = !!by)
    } else if (dot_type %in% c("character", "logical", "factor")) {
      .data <- eval_tidy(expr(
        .data %>%
          dt_mutate(na_index = 1:.N, by = !!by) %>%
          dt_mutate(na_index = fifelse(is.na(!!dot), NA_integer_, na_index)) %>%
          dt_mutate(na_index = nafill(na_index, type = "nocb"), by = !!by) %>%
          dt(, !!dot := .SD[, !!dot][na_index], by = !!by) %>%
          dt(, na_index := NULL) %>%
          dt()
      ))
    }
  }
  .data
}
