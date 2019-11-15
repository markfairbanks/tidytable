set_colon_equal_alias <- function(alias){
  base::requireNamespace("data.table")
  temp <- data.table:::`[.data.table`
  base::body(temp)[-1] <- base::parse(text = base::gsub(
    '== ":="',
    base::paste("%in%", base::deparse(c(":=", alias))) ,
    base::body(temp)[-1],
    fixed = TRUE))
  utils::assignInNamespace("[.data.table", temp, "data.table")
  base::invisible()
}

set_colon_equal_alias("let")

agg <- base::list
vars <- base::list
