replace_walrus_alias <- function(e) {
  if (is.call(e) && !is.function(e[[1L]])) {
    if (e[[1L]] == "let")
      e[[1L]] = quote(`:=`)
    for (i in seq_along(e)[-1L]) if (!is.null(e[[i]]))
      e[[i]] = replace_walrus_alias(e[[i]])
  }
  e
}

set_colon_equal_alias <- function(){
  base::requireNamespace("data.table")
  temp <- data.table:::`[.data.table`
  base::body(temp)[-1] <- base::parse(text = base::gsub(
    'replace_dot_alias(substitute(j))',
    'gdt:::replace_walrus_alias(replace_dot_alias(substitute(j)))' ,
    base::body(temp)[-1],
    fixed = TRUE))
  utils::assignInNamespace("[.data.table", temp, "data.table")
  base::invisible()
}


