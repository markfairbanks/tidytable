#' Join two data.tables together
#'
#' @description Join two data.tables together
#'
#' @param x A data.frame or data.table
#' @param y A data.frame or data.table
#' @param by A character vector of variables to join by. If NULL, the default, the join will do a natural join, using all variables with common names across the two tables.
#' @param suffix Append created for duplicated column names when using `full_join.()`
#'
#' @return A data.table
#' @export
#'
#' @examples
#' df1 <- data.table::data.table(x = c("a","a","a","b","b"), y = 1:5)
#' df2 <- data.table::data.table(x = c("a","b"), z = 1:2)
#'
#' df1 %>% left_join.(df2)
#' df1 %>% inner_join.(df2)
#' df1 %>% right_join.(df2)
#' df1 %>% full_join.(df2)
#' df1 %>% anti_join.(df2)
left_join. <- function(x, y, by = NULL) {
  UseMethod("left_join.")
}

#' @export
left_join..default <- function(x, y, by = NULL) {
  if (!is.data.frame(x) | !is.data.frame(y)) stop("x & y must be a data.frame or data.table")
  if (!is.data.table(x)) x <- as_tidytable(x)
  if (!is.data.table(y)) y <- as_tidytable(y)

  by_x_y <- get_bys(x, y, by)

  by_x <- by_x_y[[1]]
  by_y <- by_x_y[[2]]

  on_vec <- by_x
  names(on_vec) <- by_y

  y_names <- names(y)
  all_names <- c(names(x), y_names[!y_names %in% names(x)])

  as_tidytable(setcolorder(y[x, on = on_vec, allow.cartesian = TRUE], all_names))
}

#' @export
#' @rdname left_join.
inner_join. <- function(x, y, by = NULL) {
  UseMethod("inner_join.")
}

#' @export
inner_join..default <- function(x, y, by = NULL) {
  if (!is.data.frame(x) | !is.data.frame(y)) stop("x & y must be a data.frame or data.table")
  if (!is.data.table(x)) x <- as_tidytable(x)
  if (!is.data.table(y)) y <- as_tidytable(y)

  by_x_y <- get_bys(x, y, by)

  by_x <- by_x_y[[1]]
  by_y <- by_x_y[[2]]

  on_vec <- by_y
  names(on_vec) <- by_x

  as_tidytable(x[y, on = on_vec, allow.cartesian = TRUE, nomatch = 0])
}

#' @export
#' @rdname left_join.
right_join. <- function(x, y, by = NULL) {
  UseMethod("right_join.")
}

#' @export
right_join..default <- function(x, y, by = NULL) {
  if (!is.data.frame(x) | !is.data.frame(y)) stop("x & y must be a data.frame or data.table")
  if (!is.data.table(x)) x <- as_tidytable(x)
  if (!is.data.table(y)) y <- as_tidytable(y)

  by_x_y <- get_bys(x, y, by)

  by_x <- by_x_y[[1]]
  by_y <- by_x_y[[2]]

  on_vec <- by_y
  names(on_vec) <- by_x

  as_tidytable(x[y, on = on_vec, allow.cartesian = TRUE])
}

#' @export
#' @rdname left_join.
full_join. <- function(x, y, by = NULL, suffix = c(".x", ".y")) {
  UseMethod("full_join.")
}

#' @export
full_join..default <- function(x, y, by = NULL, suffix = c(".x", ".y")) {
    join_mold(x, y, by = by, suffix = suffix,
              all_x = TRUE, all_y = TRUE)

}

#' @export
#' @rdname left_join.
anti_join. <- function(x, y, by = NULL) {
  UseMethod("anti_join.")
}

#' @export
anti_join..default <- function(x, y, by = NULL) {
  if (!is.data.frame(x) | !is.data.frame(y)) stop("x & y must be a data.frame or data.table")
  if (!is.data.table(x)) x <- as_tidytable(x)
  if (!is.data.table(y)) y <- as_tidytable(y)

  by_x_y <- get_bys(x, y, by)

  by_x <- by_x_y[[1]]
  by_y <- by_x_y[[2]]

  on_vec <- by_y
  names(on_vec) <- by_x

  as_tidytable(x[!y, on = on_vec, allow.cartesian = TRUE])
}

get_bys <- function(x, y, by = NULL) {

  if (is.null(by)) {
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

  list(by_x, by_y)
}

#' @export
#' @rdname left_join.
dt_left_join <- left_join.

#' @export
#' @rdname left_join.
dt_inner_join <- inner_join.

#' @export
#' @rdname left_join.
dt_right_join <- right_join.

#' @export
#' @rdname left_join.
dt_full_join <- full_join.

#' @export
#' @rdname left_join.
dt_anti_join <- anti_join.

join_mold <- function(x, y, by = NULL, suffix = c(".x", ".y"), all_x, all_y) {
  if (!is.data.frame(x) | !is.data.frame(y)) stop("x & y must be a data.frame or data.table")
  if (!is.data.table(x)) x <- as_tidytable(x)
  if (!is.data.table(y)) y <- as_tidytable(y)

  by_x_y <- get_bys(x, y, by)

  by_x <- by_x_y[[1]]
  by_y <- by_x_y[[2]]

  if (by_x %notin% colnames(x)) stop("by.x columns not in x")
  if (by_y %notin% colnames(y)) stop("by.y columns not in y")

  as_tidytable(
    merge(x = x, y = y, by.x = by_x, by.y = by_y, suffixes = suffix,
          all.x = all_x, all.y = all_y,
          allow.cartesian = TRUE)
  )
}
