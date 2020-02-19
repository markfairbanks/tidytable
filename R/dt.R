#' Pipeable data.table call
#'
#' @description
#' Pipeable data.table call
#'
#' @param .data A data.frame or data.table
#' @param i See ?data.table:::`[.data.table`
#' @param j See ?data.table:::`[.data.table`
#' @param by See ?data.table:::`[.data.table`
#' @param keyby See ?data.table:::`[.data.table`
#' @param with See ?data.table:::`[.data.table`
#' @param nomatch See ?data.table:::`[.data.table`
#' @param mult See ?data.table:::`[.data.table`
#' @param roll See ?data.table:::`[.data.table`
#' @param rollends See ?data.table:::`[.data.table`
#' @param which See ?data.table:::`[.data.table`
#' @param .SDcols See ?data.table:::`[.data.table`
#' @param verbose See ?data.table:::`[.data.table`
#' @param allow.cartesian See ?data.table:::`[.data.table`
#' @param drop See ?data.table:::`[.data.table`
#' @param on See ?data.table:::`[.data.table`
#'
#' @export
#' @examples
#' example_dt <- data.table::data.table(
#'   x = c(1,2,3),
#'   y = c(4,5,6),
#'   z = c("a", "a", "b"))
#'
#' example_dt %>%
#'   dt(, ':='(double_x = x * 2)) %>%
#'   dt(order(-double_x))
dt <- function(.data, i, j, by, keyby, with = TRUE,
               nomatch = getOption("datatable.nomatch", NA),
               mult = "all",
               roll = FALSE,
               rollends = if (roll=="nearest") c(TRUE,TRUE)
               else if (roll>=0) c(FALSE,TRUE)
               else c(TRUE,FALSE),
               which = FALSE,
               .SDcols,
               verbose = getOption("datatable.verbose"),                   # default: FALSE
               allow.cartesian = getOption("datatable.allow.cartesian"),   # default: FALSE
               drop = NULL, on = NULL) {

  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")

  sys_call <- sys.call()

  sys_call[[1]] <- as.symbol("[")

  if (!is.data.table(.data))
    sys_call[[2]] <- substitute(as.data.table(.data))

  eval_tidy(sys_call, env = caller_env())
}
