#' Generate a unique id for consecutive values
#'
#' @description
#' Generate a unique id for runs of consecutive values
#'
#' @param ... Vectors of values
#'
#' @export
#'
#' @examples
#' vec <- c(1, 1, 2, 2, 1, 1)
#' consecutive_id.(x)
consecutive_id. <- function(...) {
  check_dots_unnamed()

  data <- data_frame(..., .name_repair = "minimal")

  rleidv(data)
}
