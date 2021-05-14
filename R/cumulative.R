#' Cumulative versions of any, all, and mean
#'
#' @description
#' `cumall.()`, `cumany.()`, and `cummean.()` to work alongside base `cumsum()`
#'
#' @param x For `cumall.()` and `cumany.()` a logical vector.
#' For `cummean.()` and integer or numeric vector.
#'
#' @examples
#' x <- c(1, 3, 5, 2, 2)
#' cummean.(x)
#'
#' # `cumall()` and `cumany()` return logicals
#' cumall.(x < 5)
#' cumany.(x == 3)
#'
#' # `cumall()` vs. `cumany()
#' df <- data.table(
#'   date = as.Date("2020-01-01") + 0:6,
#'   balance = c(100, 50, 25, -25, -50, 30, 120)
#' )
#'
#' # all rows after first overdraft
#' df %>% filter.(cumany.(balance < 0))
#'
#' # all rows until first overdraft
#' df %>% filter.(cumall.(!(balance < 0)))
#' @keywords internal
# cumall. <- function(x) {
#   end <- length(x)
#   if (is.na(all(x))) return(rep(NA, end))
#   out <- rep(TRUE, end)
#   which_not_x <- which(!x)
#   which_na_x <- which(is.na(x))
#   if (length(which_not_x) > 0) {
#     which_not_x <- which_not_x[[1]]
#     if (length(which_na_x) > 0) {
#       which_na_x <- which_na_x[[1]]
#       start <- min(which_na_x, which_not_x)
#     } else {
#       start <- which_not_x
#     }
#
#     out[start:end] <- FALSE
#
#     if (identical(which_na_x, start)) {
#       out[which_na_x] <- NA
#     }
#   }
#   out
# }

#' @keywords internal
#' @rdname cumall.
# cumany. <- function(x) {
#   end <- length(x)
#   if (is.na(all(x))) return(rep(NA, end))
#   out <- rep(FALSE, end)
#   which_x <- which(x)
#   which_na_x <- which(is.na(x))
#   if (length(which_x) > 0) {
#     which_x <- which_x[[1]]
#     if (length(which_na_x) > 0) {
#       which_na_x <- which_na_x[[1]]
#       start <- min(which_na_x, which_x)
#     } else {
#       start <- which_x
#     }
#
#     out[start:end] <- TRUE
#
#     if (identical(which_na_x, start)) {
#       out[which_na_x] <- NA
#     }
#   }
#   out
# }

#' @keywords internal
#' @rdname cumall.
# cummean. <- function(x) {
#   cumsum(x)/seq_along(x)
# }
