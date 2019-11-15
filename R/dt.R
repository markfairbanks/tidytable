#' One-to-one interface for data.table '[' method
#'
#' Quote from \link[data.table]{data.table}:
#' \preformatted{
#' dt(data, i,  j,  by) # + extra arguments
#'              |   |   |
#'              |   |    -------> grouped by what?
#'              |    -------> what to do?
#'               ---> on which rows?
#' }
#'
#' @param data data.table/data.frame data.frame will be automatically converted
#'   to data.table.
#' @param i Integer, logical or character vector, single column numeric matrix,
#'   expression of column names, list, data.frame or data.table. integer and
#'   logical vectors work the same way they do in [.data.frame except logical
#'   NAs are treated as FALSE. expression is evaluated within the frame of the
#'   data.table (i.e. it sees column names as if they are variables) and can
#'   evaluate to any of the other types. For details see
#'   \link[data.table]{data.table}
#' @param j When with=TRUE (default), j is evaluated within the frame of the
#'   data.table; i.e., it sees column names as if they are variables. This
#'   allows to not just select columns in j, but also compute on them e.g., x[,
#'   a] and x[, sum(a)] returns x$a and sum(x$a) as a vector respectively. x[,
#'   .(a, b)] and x[, .(sa=sum(a), sb=sum(b))] returns a two column data.table
#'   each, the first simply selecting columns a, b and the second computing
#'   their sums. For details see \link[data.table]{data.table}.
#' @param by unquoted name of grouping variable of list of unquoted names of
#'   grouping variables. For details see \link[data.table]{data.table}
#' @param keyby Same as by, but with an additional \code{setkey()} run on the by
#'   columns of the result, for convenience. It is common practice to use
#'   'keyby=' routinely when you wish the result to be sorted. For details see
#'   \link[data.table]{data.table}
#' @param with logical. For details see \link[data.table]{data.table}.
#' @param nomatch Same as nomatch in match. For details see
#'   \link[data.table]{data.table}.
#' @param mult For details see \link[data.table]{data.table}.
#' @param roll For details see \link[data.table]{data.table}.
#' @param rollends For details see \link[data.table]{data.table}.
#' @param which For details see \link[data.table]{data.table}.
#' @param .SDcols Specifies the columns of x to be included in the special
#'   symbol .SD which stands for Subset of data.table. May be character column
#'   names or numeric positions. For details see \link[data.table]{data.table}.
#' @param verbose logical. For details see \link[data.table]{data.table}.
#' @param allow.cartesian For details see \link[data.table]{data.table}.
#' @param drop For details see \link[data.table]{data.table}.
#' @param on For details see \link[data.table]{data.table}.
#'
#' @return It depends. For details see \link[data.table]{data.table}.
#' @export
#'
#' @examples
#' example_dt <- data.table(x = c(1,2,3), y = c(4,5,6), z = c("a", "a", "b"))
#' example_dt %>%
#'   dt(, let(double_x = x * 2)) %>%
#'   dt(order(-double_x)) %>%
#'   dt(, agg(avg_x = mean(x)), by = z)
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
                verbose = getOption("datatable.verbose"),                   # default: FALSE
                allow.cartesian = getOption("datatable.allow.cartesian"),   # default: FALSE
                drop = NULL,
                on = NULL){
  is.data.frame(data) || stop("dt: 'data' should be data.frame or data.table")
  call_expr = sys.call()
  if(!is.data.table(data)){
    call_expr[[2]] = substitute(as.data.table(data))
  }
  call_expr[[1]] = as.symbol("[")
  eval.parent(call_expr)
}
