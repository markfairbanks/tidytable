vec_selector_i <- function(.data, select_vars) {

  data_names <- colnames(.data)
  data_vars <- setNames(as.list(seq_along(.data)), data_names)

  integer_cols <- list(is.integer = seq_along(data_names)[dt_map_lgl(.data, is.integer)])
  double_cols <- list(is.double = seq_along(data_names)[dt_map_lgl(.data, is.double)])
  numeric_cols <- list(is.numeric = seq_along(data_names)[dt_map_lgl(.data, is.numeric)])
  character_cols <- list(is.character = seq_along(data_names)[dt_map_lgl(.data, is.character)])
  factor_cols <- list(is.factor = seq_along(data_names)[dt_map_lgl(.data, is.factor)])
  logical_cols <- list(is.logical = seq_along(data_names)[dt_map_lgl(.data, is.logical)])

  data_vars <- data_vars %>%
    append(integer_cols) %>%
    append(double_cols) %>%
    append(numeric_cols) %>%
    append(character_cols) %>%
    append(factor_cols) %>%
    append(logical_cols)

  select_vars <- enexpr(select_vars)
  select_index <- unlist(eval(expr(c(!!select_vars)), data_vars))

  keep_index <- unique(select_index[select_index > 0])
  if (length(keep_index) == 0) keep_index <- seq_along(.data)
  drop_index <- unique(abs(select_index[select_index < 0]))

  select_index <- setdiff(keep_index, drop_index)

  select_index
}

vec_selector <- function(.data, select_vars) {
  select_vars <- enexpr(select_vars)

  select_index <- vec_selector_i(.data, !!select_vars)
  data_names <- colnames(.data)

  select_vars <- data_names[select_index] %>%
    as.list() %>%
    dt_map(sym)

  select_vars
}

dots_selector_i <- function(.data, ...) {

  data_names <- colnames(.data)
  data_vars <- setNames(as.list(seq_along(.data)), data_names)

  integer_cols <- list(is.integer = seq_along(data_names)[dt_map_lgl(.data, is.integer)])
  double_cols <- list(is.double = seq_along(data_names)[dt_map_lgl(.data, is.double)])
  numeric_cols <- list(is.numeric = seq_along(data_names)[dt_map_lgl(.data, is.numeric)])
  character_cols <- list(is.character = seq_along(data_names)[dt_map_lgl(.data, is.character)])
  factor_cols <- list(is.factor = seq_along(data_names)[dt_map_lgl(.data, is.factor)])
  logical_cols <- list(is.logical = seq_along(data_names)[dt_map_lgl(.data, is.logical)])

  data_vars <- data_vars %>%
    append(integer_cols) %>%
    append(double_cols) %>%
    append(numeric_cols) %>%
    append(character_cols) %>%
    append(factor_cols) %>%
    append(logical_cols)

  select_vars <- enexprs(...)
  select_index <- unlist(eval(expr(c(!!!select_vars)), data_vars))

  keep_index <- unique(select_index[select_index > 0])
  if (length(keep_index) == 0) keep_index <- seq_along(.data)
  drop_index <- unique(abs(select_index[select_index < 0]))

  select_index <- setdiff(keep_index, drop_index)

  select_index
}

dots_selector <- function(.data, ...) {

  select_index <- dots_selector_i(.data, ...)
  data_names <- colnames(.data)

  select_vars <- data_names[select_index] %>%
    as.list() %>%
    dt_map(sym)

  select_vars
}
