#' Unnest a nested data.table
#'
#' @param .data A nested data.table
#' @param col The column to unnest
#' @param ... Column names to keep
#'
#' @return A data.table
#' @export
#'
#' @examples
#' test_df <- data.table(a = 1:10,
#'                       b = 11:20,
#'                       c = c(rep("a", 6), rep("b", 4)),
#'                       d = c(rep("a", 4), rep("b", 6)))
#' test_df %>%
#'   dt_group_nest(c, d) %>%
#'   dt_unnest(data, c, d)
dt_unnest_legacy <- function(.data, col, ...) {
  col <- enexpr(col)
  dots <- dots_selector(.data, ...)

  if (length(dots) > 0) {
    .data <- .data %>%
      as_dt() %>%
      dt_mutate(.count = dt_map(!!col, get_length))

    result_list <- dt_map(
      dots,
      function(dot) {
        data.table(.unnamed_col = dt_map2(dt_pull(.data, !!dot),
                                          dt_pull(.data, .count),
                                          function(.x, .y) rep(.x, .y)) %>%
                     unname() %>%
                     unlist())})

    keep_df <- do.call(cbind, result_list)

    names(keep_df) <- as.character(dots)

    unnest_list <- .data %>%
      dt_pull(!!col)

    if ("data.frame" %in% class(unnest_list[[1]])) {
      # If data.frame rbindlist() & cbind() to keep_df
      unnest_list <- rbindlist(unnest_list)

      return(cbind(keep_df, unnest_list))
    } else {
      # If vec add as a new column to keep_df
      keep_df[[as.character(col)]] <- unlist(unnest_list)

      return(keep_df)
    }
  } else {
    # If dots are empty, do a simple unnest
    unnest_df <- .data %>%
      dt_pull(!!col) %>%
      rbindlist()

    return(unnest_df)
  }
}

get_length <- function(x) {
  if ("data.frame" %in% class(x)) {
    nrow(x)
  } else {
    length(x)
  }
}
