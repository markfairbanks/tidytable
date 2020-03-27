# vec_selector & dots_selector return a list of bare column names
# vec_selector_i & dots_selector_i return column positions/index as an integer vector
# vec_selector_by & dots_selector_by return a list() that can be unquoted in a data.table call


### Get group by cols

vec_selector_by <- function(.data, by_vars) {
  by_vars <- enexpr(by_vars)

  if(is.null(by_vars)) {
    by_vars <- NULL
  } else {
    by_vars <- vec_selector(.data, !!by_vars)

    by_vars <- expr(list(!!!by_vars))
  }
  by_vars
}

dots_selector_by <- function(.data, ...) {
  dots <- enexprs(...)

  if(length(dots) == 0) {
    by_vars <- NULL
  } else {
    by_vars <- dots_selector(.data, ...)

    by_vars <- expr(list(!!!by_vars))
  }
  by_vars
}

### User inputs a vector of bare column names

vec_selector <- function(.data, select_vars) {
  select_vars <- enexpr(select_vars)

  select_index <- vec_selector_i(.data, !!select_vars)
  data_names <- names(.data)

  select_vars <- syms(data_names[select_index])

  select_vars
}

vec_selector_i <- function(.data, select_vars) {

  data_vars <- get_data_vars(.data)

  select_vars <- enexpr(select_vars)
  select_index <- unlist(eval(expr(c(!!select_vars)), data_vars))

  keep_index <- unique(select_index[select_index > 0])
  if (length(keep_index) == 0) keep_index <- seq_along(.data)
  drop_index <- unique(abs(select_index[select_index < 0]))

  select_index <- keep_index[!keep_index %in% drop_index]

  select_index
}

### User inputs bare column names using ellipsis

dots_selector <- function(.data, ...) {

  select_index <- dots_selector_i(.data, ...)
  data_names <- names(.data)

  select_vars <- syms(data_names[select_index])

  select_vars
}

dots_selector_i <- function(.data, ...) {

  data_vars <- get_data_vars(.data)

  select_vars <- enexprs(...)
  select_index <- unlist(eval(expr(c(!!!select_vars)), data_vars))

  keep_index <- unique(select_index[select_index > 0])
  if (length(keep_index) == 0) keep_index <- seq_along(.data)
  drop_index <- unique(abs(select_index[select_index < 0]))

  select_index <- keep_index[!keep_index %in% drop_index]

  select_index
}

#### Get data variables
get_data_vars <- function(.data) {

  data_names <- names(.data)
  data_index <- seq_along(data_names)
  data_vars <- setNames(as.list(data_index), data_names)
  data_class <- map_chr.(.data, class)

  integer_cols <- list(is.integer = data_index[data_class == "integer"])
  double_cols <- list(is.double = data_index[data_class == "numeric"])
  numeric_cols <- list(is.numeric = data_index[data_class %in% c("integer", "numeric")])
  character_cols <- list(is.character = data_index[data_class == "character"])
  factor_cols <- list(is.factor = data_index[data_class == "factor"])
  logical_cols <- list(is.logical = data_index[data_class == "logical"])
  list_cols <- list(is.list = data_index[data_class == "list"])

  data_vars <- data_vars %>%
    append(integer_cols) %>%
    append(double_cols) %>%
    append(numeric_cols) %>%
    append(character_cols) %>%
    append(factor_cols) %>%
    append(logical_cols) %>%
    append(list_cols)

  data_vars
}
