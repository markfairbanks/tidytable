# Testing
# join_spec <- join_by(x, y >= z)
# prep_join_by(join_spec)

# TODO (working list)
#  * Move join_by to separate file
#  * Renaming columns pre-join?
#  * Review dplyr semantics with keep = FALSE & keep = TRUE
#  * What to do with between/overlaps/etc.

# Need to export/create separate file
join_by <- function(...) {
  dots <- enquos(...)
  dots <- map(dots, quo_squash)

  set_class(dots, c("tidytable_join_by", "list"))
}

is_join_by <- function(x) {
  inherits(x, "tidytable_join_by")
}

prep_join_by <- function(x, type = "left") {
  if (type == "left") {
    x <- flip_inputs(x)
  }
  on <- call2(".", !!!x)
  on
}

flip_inputs <- function(x) {
  map(x, flip_input)
}

flip_input <- function(x) {
  if (is_call(x, c("==", "<=", ">=", ">", "<"))) {
    out <- flip_operator(x)
    out[[3]] <- x[[2]]
    out[[2]] <- x[[3]]
    out
  } else {
    out <- x
  }
  out
}

flip_operator <- function(x) {
  old_operator <- as.character(x[[1]])
  new_operator <- case_match(
    old_operator,
    "<=" ~ ">=",
    ">=" ~ "<=",
    ">" ~ "<",
    "<" ~ ">",
    .default = old_operator
  )
  x[[1]] <- sym(new_operator)
  x
}
