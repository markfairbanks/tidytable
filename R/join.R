#' Join two data.tables together
#'
#' @description Join two data.tables together
#'
#' @usage
#' dt_left_join(x, y, by = NULL, suffix = c(".x", ".y"))
#' dt_inner_join(x, y, by = NULL, suffix = c(".x", ".y"))
#' dt_right_join(x, y, by = NULL, suffix = c(".x", ".y"))
#' dt_full_join(x, y, by = NULL, suffix = c(".x", ".y"))
#'
#' @param x A data.frame or data.table
#' @param y A data.frame or data.table
#' @param by A character vector of variables to join by. If NULL, the default, the join will do a natural join, using all variables with common names across the two tables.
#' @param suffix
#'
#' @return A data.table
#' @export
#'
#' @examples
#' band_members %>% dt_left_join(band_instruments)
#' band_members %>% dt_inner_join(band_instruments)
#' band_members %>% dt_right_join(band_instruments)
#' band_members %>% dt_full_join(band_instruments)
dt_left_join <- function(x, y, by = NULL, suffix = c(".x", ".y")) {
  join_mold(x, y, by = by, suffix = suffix,
            all_x = TRUE, all_y = FALSE)
}

#' @export
#' @inherit dt_left_join
dt_inner_join <- function(x, y, by = NULL, suffix = c(".x", ".y")) {
  join_mold(x, y, by = by, suffix = suffix,
            all_x = FALSE, all_y = FALSE)
}

#' @export
#' @inherit dt_left_join
dt_right_join <- function(x, y, by = NULL, suffix = c(".x", ".y")) {
  join_mold(x, y, by = by, suffix = suffix,
            all_x = FALSE, all_y = TRUE)
}

#' @export
#' @inherit dt_left_join
dt_full_join <- function(x, y, by = NULL, suffix = c(".x", ".y")) {
  join_mold(x, y, by = by, suffix = suffix,
            all_x = TRUE, all_y = TRUE)
}

join_mold <- function(x, y, by = NULL, suffix = c(".x", ".y"), all_x, all_y) {
  if (!is.data.frame(x) | !is.data.frame(y)) stop("x & y must be a data.frame or data.table")
  if (!is.data.table(x)) x <- as.data.table(x)
  if (!is.data.table(y)) y <- as.data.table(y)

  if (missing(by)) {
    by_x <- by_y <- intersect(colnames(x), colnames(y))
  } else {
    by_x <- names(by)
    by_y <- unname(by)
    if (is.null(by_x)) {
      by_x <- by_y
    }
  }

  if (by_x %notin% colnames(x)) stop("by.x columns not in x")
  if (by_y %notin% colnames(y)) stop("by.y columns not in y")

  merge(x = x, y = y, by.x = by_x, by.y = by_y, suffixes = suffix,
        all.x = all_x, all.y = all_y,
        allow.cartesian = TRUE)
}
