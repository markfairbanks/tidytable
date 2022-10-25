#' Ranking functions
#'
#' @description
#' Ranking functions:
#' * `row_number()`: Gives other row number if empty.
#'   Equivalent to `frank(ties.method = "first")` if provided a vector.
#' * `min_rank()`: Equivalent to `frank(ties.method = "min")`
#' * `dense_rank()`: Equivalent to `frank(ties.method = "dense")`
#' * `percent_rank()`: Ranks by percentage from 0 to 1
#' * `cume_dist()`: Cumulative distribution
#'
#' @param x A vector to rank
#'
#' @export
#'
#' @examples
#' df <- data.table(x = rep(1, 3), y = c("a", "a", "b"))
#'
#' df %>%
#'   mutate(row = row_number())
#' @rdname rank
row_number <- function(x) {
  if (is.data.frame(x)) {
    vec_rank(x, ties = "sequential", incomplete = "na")
  } else {
    frank(x, ties.method = "first", na.last = "keep")
  }
}

#' @export
#' @keywords internal
#' @inherit row_number title description examples
row_number. <- row_number

#' @export
#' @rdname rank
min_rank <- function(x) {
  if (is.data.frame(x)) {
    vec_rank(x, ties = "min", incomplete = "na")
  } else {
    frank(x, ties.method = "min", na.last = "keep")
  }
}

#' @export
#' @rdname rank
dense_rank <- function(x) {
  if (is.data.frame(x)) {
    vec_rank(x, ties = "dense", incomplete = "na")
  } else {
    frank(x, ties.method = "dense", na.last = "keep")
  }
}

#' @export
#' @rdname rank
percent_rank <- function(x) {
  (min_rank(x) - 1) / (sum(vec_detect_complete(x)) - 1)
}

#' @export
#' @rdname rank
cume_dist <- function(x) {
  if (is.data.frame(x)) {
    vec_rank(x, ties = "max", incomplete = "na") / sum(vec_detect_complete(x))
  } else {
    frank(x, ties.method = "max", na.last = "keep") / sum(vec_detect_complete(x))
  }
}
