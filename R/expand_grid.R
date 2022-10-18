#' Create a data.table from all combinations of inputs
#'
#' @description
#' Create a data.table from all combinations of inputs
#'
#' @param ... Variables to get combinations of
#' @param .name_repair Treatment of problematic names. See `?vctrs::vec_as_names` for options/details
#'
#' @export
#'
#' @examples
#' x <- 1:2
#' y <- 1:2
#'
#' expand_grid(x, y)
#'
#' expand_grid(stuff = x, y)
expand_grid <- function(..., .name_repair = "check_unique") {
  dots <- dots_list(..., .named = TRUE)

  if (any(map_lgl(dots, is.data.frame))) {
    df_expand_grid(!!!dots, .name_repair = .name_repair)
  } else {
    cj_expand_grid(!!!dots, .name_repair = .name_repair)
  }
}

#' @export
#' @keywords internal
#' @inherit expand_grid
expand_grid. <- expand_grid

cj_expand_grid <- function(..., .name_repair = "check_unique") {
  out <- exec(CJ, ..., unique = FALSE, sorted = FALSE)

  out <- df_name_repair(out, .name_repair)

  as_tidytable(out)
}

df_expand_grid <- function(..., .name_repair = "check_unique") {
  l <- list2(...)

  sizes <- list_sizes(l)

  size <- prod(sizes)

  if (size == 0) {
    out <- map(l, vec_slice, integer())
  } else {
    each <- size / cumprod(sizes)
    times <- size / each / sizes
    l_names <- names(l) %||% as.character(seq_along(l))

    out <- pmap(
      list(x = l, each = each, times = times, x_name = l_names),
      make_cj_tidytable
    )
  }

  out <- df_list(!!!out, .name_repair = .name_repair)
  new_tidytable(out)
}

make_cj_tidytable <- function(x, each, times, x_name) {
  out <- vec_rep(vec_rep_each(x, each), times = times)
  if (!is.data.frame(out)) {
    # If `out` is a vector make it a tidytable
    out <- tidytable(!!x_name := out)
  }
  out
}
