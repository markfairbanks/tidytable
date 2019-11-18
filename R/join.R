#' Join two data.frames by common columns.
#'
#' Do different versions of SQL join operations. See examples.
#'
#' @param x data.frame or data.table
#' @param y data.frame or data.table
#' @param by a character vector of variables to join by. If NULL, the default,
#'   *_join() will do a natural join, using all variables with common names
#'   across the two tables. A message lists the variables so that you can check
#'   they're right (to suppress the message, simply explicitly list the
#'   variables that you want to join). To join by different variables on x and y
#'   use a named vector. For example, by = c("a" = "b") will match x.a to y.b.
#' @param suffix If there are non-joined duplicate variables in x and y, these
#'   suffixes will be added to the output to disambiguate them. Should be a
#'   character vector of length 2.
#'
#' @return data.table
#' @export
#'
#' @examples
#' workers = fread("
#'     name company
#'     Nick Acme
#'     John Ajax
#'     Daniela Ajax
#' ")
#'
#' positions = fread("
#'     name position
#'     John designer
#'     Daniela engineer
#'     Cathie manager
#' ")
#'
#' workers %>% dt_inner_join(positions)
#' workers %>% dt_left_join(positions)
#' workers %>% dt_right_join(positions)
#' workers %>% dt_full_join(positions)
#'
#' # filtering joins
#' workers %>% dt_anti_join(positions)
#' workers %>% dt_semi_join(positions)
#'
#' # To suppress the message, supply 'by' argument
#' workers %>% dt_left_join(positions, by = "name")
#'
#' # Use a named 'by' if the join variables have different names
#' positions2 = setNames(positions, c("worker", "position")) # rename first column in 'positions'
#' workers %>% dt_inner_join(positions2, by = c("name" = "worker"))
dt_left_join = function (x, y, by = NULL, suffix = c(".x", ".y")) {
  dt_join(x = x,
          y = y,
          by = by,
          suffix = suffix,
          all_x = TRUE,
          all_y = FALSE,
          src = "dt_left_join"
  )
}

#' @export
#' @rdname dt_left_join
dt_right_join = function (x, y, by = NULL, suffix = c(".x", ".y")) {
  dt_join(x = x,
          y = y,
          by = by,
          suffix = suffix,
          all_x = FALSE,
          all_y = TRUE,
          src = "dt_right_join"
  )
}

#' @export
#' @rdname dt_left_join
dt_inner_join = function (x, y, by = NULL, suffix = c(".x", ".y")) {
  dt_join(x = x,
          y = y,
          by = by,
          suffix = suffix,
          all_x = FALSE,
          all_y = FALSE,
          src = "dt_inner_join"
  )
}

#' @export
#' @rdname dt_left_join
dt_full_join = function (x, y, by = NULL, suffix = c(".x", ".y")) {
  dt_join(x = x,
          y = y,
          by = by,
          suffix = suffix,
          all_x = TRUE,
          all_y = TRUE,
          src = "dt_full_join"
  )
}

#' @export
#' @rdname dt_left_join
dt_semi_join = function (x, y, by = NULL) {
  if(!is.data.table(x)) x = as.data.table(x)
  if(!is.data.table(y)) y = as.data.table(y)
  if(length(by)==0) {
    by = get_column_names(colnames(x), colnames(y), src = "dt_semi_join")
  }
  fsetdiff(x, x[!y, on=by], all=TRUE)
}

#' @export
#' @rdname dt_left_join
dt_anti_join = function (x, y, by = NULL) {
  if(!is.data.table(x)) x = as.data.table(x)
  if(!is.data.table(y)) y = as.data.table(y)
  if(length(by)==0) {
    by = get_column_names(colnames(x), colnames(y), src = "dt_anti_join")
  }
  by_x = names(by)
  by_y = unname(by)
  if(is.null(by_x)){
    by_x = by_y
  } else {
    empty_names = is.na(by_x)|(by_x=="")
    by_x[empty_names] = by_y[empty_names]
  }
  check_existense(by_x, by_y, colnames(x), colnames(y), src = "dt_anti_join")
  x[!y, on = by]
}

get_column_names = function(col_x, col_y, src){
  by = intersect(col_x, col_y)
  if(length(by)>0){
    message(sprintf("%s: joining, by = %s", src, paste(paste0('"', by, '"'), collapse = ", ")))
  } else {
    stop(sprintf("%s: 'by' required, because the data sources have no common variables", src), call. = FALSE)
  }
  by
}

dt_join = function(x, y, by, suffix, all_x, all_y, src){
  if(!is.data.table(x)) x = as.data.table(x)
  if(!is.data.table(y)) y = as.data.table(y)
  if(length(by)==0) {
    by_x = by_y = get_column_names(colnames(x), colnames(y), src)
  } else {
    by_x = names(by)
    by_y = unname(by)
    if(is.null(by_x)){
      by_x = by_y
    } else {
      empty_names = is.na(by_x)|(by_x=="")
      by_x[empty_names] = by_y[empty_names]
    }
  }
  check_existense(by_x, by_y, colnames(x), colnames(y), src)
  merge(x = x,
        y = y,
        by.x = by_x,
        by.y = by_y,
        all.x = all_x,
        all.y = all_y,
        suffixes = suffix,
        sort = FALSE,
        no.dups = TRUE,
        allow.cartesian = TRUE
  )

}


check_existense = function(by_x, by_y, names_x, names_y, src){
  err = setdiff(by_x, names_x)
  if(length(err)>0){
    err = paste(paste0("'", err, "'"), collapse = ", ")
    stop(sprintf("'%s': 'by' can't contain join column(s) %s which is missing from LHS", src, err), call. = FALSE)
  }
  err = setdiff(by_y, names_y)
  if(length(err)>0){
    err = paste(paste0("'", err, "'"), collapse = ", ")
    stop(sprintf("'%s': 'by' can't contain join column(s) %s which is missing from RHS", src, err), call. = FALSE)
  }
}
