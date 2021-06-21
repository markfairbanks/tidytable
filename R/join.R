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
#' df1 <- data.table(x = c("a","a","a","b","b"), y = 1:5)
#' df2 <- data.table(x = c("a","b"), z = 1:2)
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
  if (!is_tidytable(x)) x <- as_tidytable(x)
  if (!is_tidytable(y)) y <- as_tidytable(y)

  by <- get_bys(x, y, by)

  on <- by$x
  names(on) <- by$y
  on[on == ""] <- by$y[on == ""]

  # Get result names to have correct column order
  y_names <- names(y)
  y_names <- y_names[y_names %notin% names(x)]
  y_names <- y_names[y_names %notin% by$y]
  all_names <- c(names(x), y_names)

  result_df <- y[x, on = on, allow.cartesian = TRUE]

  setnames(result_df, by$y, on)
  setcolorder(result_df, all_names)

  tidytable_restore(result_df, x)
}

#' @export
#' @rdname left_join.
inner_join. <- function(x, y, by = NULL) {
  UseMethod("inner_join.")
}

#' @export
inner_join..default <- function(x, y, by = NULL) {
  if (!is.data.frame(x) | !is.data.frame(y)) stop("x & y must be a data.frame or data.table")
  if (!is_tidytable(x)) x <- as_tidytable(x)
  if (!is_tidytable(y)) y <- as_tidytable(y)

  by <- get_bys(x, y, by)

  on <- by$y
  names(on) <- by$x

  result_df <- x[y, on = on, allow.cartesian = TRUE, nomatch = 0]

  tidytable_restore(result_df, x)
}

#' @export
#' @rdname left_join.
right_join. <- function(x, y, by = NULL) {
  UseMethod("right_join.")
}

#' @export
right_join..default <- function(x, y, by = NULL) {
  if (!is.data.frame(x) | !is.data.frame(y)) stop("x & y must be a data.frame or data.table")
  if (!is_tidytable(x)) x <- as_tidytable(x)
  if (!is_tidytable(y)) y <- as_tidytable(y)

  by <- get_bys(x, y, by)

  on <- by$y
  names(on) <- by$x

  result_df <- x[y, on = on, allow.cartesian = TRUE]

  tidytable_restore(result_df, x)
}

#' @export
#' @rdname left_join.
full_join. <- function(x, y, by = NULL, suffix = c(".x", ".y")) {
  UseMethod("full_join.")
}

#' @export
full_join..default <- function(x, y, by = NULL, suffix = c(".x", ".y")) {
    result_df <- join_mold(
      x, y, by = by, suffix = suffix,
      all_x = TRUE, all_y = TRUE
    )

    start_names <- names(x)
    end_names <- names(result_df)
    end_names <- end_names[end_names %notin% start_names]

    col_order <- c(start_names, end_names)

    setcolorder(result_df, col_order)

    tidytable_restore(result_df, x)
}

#' @export
#' @rdname left_join.
anti_join. <- function(x, y, by = NULL) {
  UseMethod("anti_join.")
}

#' @export
anti_join..default <- function(x, y, by = NULL) {
  if (!is.data.frame(x) | !is.data.frame(y)) stop("x & y must be a data.frame or data.table")
  if (!is_tidytable(x)) x <- as_tidytable(x)
  if (!is_tidytable(y)) y <- as_tidytable(y)

  by <- get_bys(x, y, by)

  on <- by$y
  names(on) <- by$x

  result_df <- x[!y, on = on, allow.cartesian = TRUE]

  tidytable_restore(result_df, x)
}

#' @export
#' @rdname left_join.
semi_join. <- function(x, y, by = NULL) {
  UseMethod("semi_join.")
}

#' @export
semi_join..default <- function(x, y, by = NULL) {
  if (!is.data.frame(x) | !is.data.frame(y)) stop("x & y must be a data.frame or data.table")
  if (!is_tidytable(x)) x <- as_tidytable(x)
  if (!is_tidytable(y)) y <- as_tidytable(y)

  by <- get_bys(x, y, by)

  on <- by$y
  names(on) <- by$x

  result_df <- fsetdiff(x, x[!y, on = on], all=TRUE)

  tidytable_restore(result_df, x)
}

get_bys <- function(x, y, by = NULL) {
  names_x <- names(x)
  names_y <- names(y)

  if (is.null(by)) {
    by_x <- by_y <- intersect(names_x, names_y)
  } else {
    by_x <- names(by)
    by_y <- unname(by)
    if (is.null(by_x)) {
      by_x <- by_y
    }
  }

  if (any(by_x[by_x != ""] %notin% names_x)) stop("by.x columns not in x")
  if (any(by_y[by_y != ""] %notin% names_y)) stop("by.y columns not in y")

  list(x = by_x, y = by_y)
}

join_mold <- function(x, y, by = NULL, suffix = c(".x", ".y"), all_x, all_y) {
  if (!is.data.frame(x) | !is.data.frame(y)) stop("x & y must be a data.frame or data.table")
  if (!is_tidytable(x)) x <- as_tidytable(x)
  if (!is_tidytable(y)) y <- as_tidytable(y)

  by <- get_bys(x, y, by)

  result_df <- merge(
    x = x, y = y, by.x = by$x, by.y = by$y, suffixes = suffix,
    all.x = all_x, all.y = all_y, allow.cartesian = TRUE, sort = FALSE
  )

  setkey(result_df, NULL)

  result_df
}
