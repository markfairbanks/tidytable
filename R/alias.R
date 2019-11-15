set_colon_equal_alias <- function(alias){
  requireNamespace("data.table")
  temp <- data.table:::`[.data.table`
  body(temp)[-1] <- parse(text = gsub(
    '== ":="',
    paste("%in%", deparse(c(":=", alias))) ,
    body(temp)[-1],
    fixed = TRUE))
  assignInNamespace("[.data.table", temp, "data.table")
  invisible()
}

set_colon_equal_alias("let")

agg <- list
vars <- list
