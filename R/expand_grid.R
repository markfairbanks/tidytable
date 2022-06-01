#' Create a data.table from all combinations of inputs
#'
#' @description
#' Create a data.table from all combinations of inputs
#'
#' @param ... Variables to get combinations of
#' @param .name_repair Treatment of problematic names. See `?vctrs::vec_as_names` for options/details
#'
#' @md
#' @export
#'
#' @examples
#' x <- 1:2
#' y <- 1:2
#'
#' expand_grid.(x, y)
#'
#' expand_grid.(stuff = x, y)
expand_grid. <- function(..., .name_repair = "check_unique") {
  dots <- dots_list(..., .named = TRUE)

  if (any(map_lgl.(dots, is.data.frame))) {
    expand_grid_df(!!!dots, .name_repair = .name_repair)
  } else {
    expand_grid_vec(!!!dots, .name_repair = .name_repair)
  }
}

expand_grid_vec <- function(..., .name_repair = "check_unique") {
  dots <- list2(...)

  result_df <- exec(CJ, !!!dots, unique = FALSE, sorted = FALSE)

  setkey(result_df, NULL)

  result_df <- df_name_repair(result_df, .name_repair = .name_repair)

  as_tidytable(result_df)
}

expand_grid_df <- function(..., .name_repair = "check_unique") {
  l <- list2(...)

  sizes <- list_sizes(l)

  size <- prod(sizes)

  if (size == 0) {
    out <- map.(l, vec_slice, integer())
  } else {
    each <- size / cumprod(sizes)
    times <- size / each / sizes
    l_names <- names(l) %||% as.character(seq_along(l))

    out <- pmap.(
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
