#' Pipeable data.table call
#'
#' @export
#'
#' @examples
#' example_dt <- data.table(x = c(1,2,3), y = c(4,5,6), z = c("a", "a", "b"))
#'
#' example_dt %>%
#'   as_dt() %>%
#'   dt(, let(double_x = x * 2)) %>%
#'   dt(order(-double_x))
dt <- function(data,
                i,
                j,
                by,
                keyby,
                with = TRUE,
                nomatch = getOption("datatable.nomatch"),
                mult = "all",
                roll = FALSE,
                rollends = if (roll=="nearest") c(TRUE,TRUE)
                else if (roll>=0) c(FALSE,TRUE)
                else c(TRUE,FALSE),
                which = FALSE,
                .SDcols,
                verbose = getOption("datatable.verbose"),
                allow.cartesian = getOption("datatable.allow.cartesian"),
                drop = NULL,
                on = NULL){
  is.data.table(data) || stop("dt: 'data' should be a data.table")
  call_expr = sys.call()
  call_expr[[1]] = as.symbol("[")
  eval.parent(call_expr)
}
