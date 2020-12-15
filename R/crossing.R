#' Create a data.table from all unique combinations of inputs
#'
#' @description
#' `crossing.()` is similar to `expand_grid.()` but de-duplicates and sorts its inputs.
#'
#' @param ... Variables to get unique combinations of
#' @param .name_repair Treatment of problematic names. See `?vctrs::vec_as_names` for options/details
#'
#' @md
#' @export
#'
#' @examples
#' x <- 1:2
#' y <- 1:2
#'
#' crossing.(x, y)
#'
#' crossing.(stuff = x, y)
crossing. <- function(..., .name_repair = "check_unique") {

  dots <- list2(...)

  if (any(map_lgl.(dots, is.data.frame))) {
    crossing_df(..., .name_repair = .name_repair)
  } else {
    crossing_vec(..., .name_repair = .name_repair)
  }
}

crossing_vec <- function(..., .name_repair = "check_unique") {

  result_df <- CJ(..., sorted = TRUE, unique = TRUE)

  setkey(result_df, NULL)

  result_df <- df_name_repair(result_df, .name_repair = .name_repair)

  as_tidytable(result_df)
}

crossing_df <- function(..., .name_repair = "check_unique") {

  l <- list2(...)
  l <- map.(l, sort_unique)
  lgs <- map_int.(l, vec_size)

  lg <- prod(lgs)

  if (lg == 0) {
    out <- map.(l, vec_slice, integer())
  } else {
    each <- lg / cumprod(lgs)
    times <- lg / each / lgs
    l_names <- names(l) %||% as.character(seq_along(l))

    out <- pmap.(
      list(x = l, each = each, times = times, x_name = l_names),
      make_cj_tidytable
    )
  }

  bind_cols.(out, .name_repair = .name_repair)
}

sort_unique <- function(x) {
  if (is.factor(x)) {
    # forcats::fct_unique
    factor(levels(x), levels(x), exclude = NULL, ordered = is.ordered(x))
  } else if (is_bare_list(x)) {
    vec_unique(x)
  } else if (is.data.frame(x)) {
    setorderv(unique(as_tidytable(x)))[]
  } else {
    f_sort(vec_unique(x))
  }
}

make_cj_tidytable <- function(x, each, times, x_name) {
  if (is.data.frame(x)) {
    x[rep(1:.N, each = each, times = times)]
  } else {
    tidytable(!!x_name := rep(x, each = each, times = times))
  }
}
