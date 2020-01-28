#' Unnest a nested data.table
#'
#' @description
#' Unnest a nested data.table.
#'
#' Supports enhanced selection
#'
#' @param .data A nested data.table
#' @param col The column to unnest
#' @param keep Vector `c()` of bare column names to keep
#'
#' @return A data.table
#' @export
#'
#' @examples
#' nested_df <- data.table::data.table(
#'   a = 1:10,
#'   b = 11:20,
#'   c = c(rep("a", 6), rep("b", 4)),
#'   d = c(rep("a", 4), rep("b", 6))) %>%
#'   dt_group_nest(c, d)
#'
#' nested_df %>%
#'   dt_unnest_legacy(data, keep = c(c, d))
#'
#' nested_df %>%
#'   dt_unnest_legacy(data, keep = is.character)
dt_unnest_legacy <- function(.data, col, keep = NULL) {
  col <- enexpr(col)
  keep_cols <- enexpr(keep)

  if (!is.null(keep_cols)) {
    keep_cols <- vec_selector(.data, !!keep_cols)

    .data <- .data %>%
      as_dt() %>%
      dt_mutate(.count = dt_map(!!col, get_length))

    result_list <- dt_map(
      keep_cols,
      function(keep_col) {
        data.table(.unnamed_col = dt_map2(dt_pull(.data, !!keep_col),
                                          dt_pull(.data, .count),
                                          function(.x, .y) rep(.x, .y)) %>%
                     unname() %>%
                     unlist())})

    keep_df <- as_dt(do.call(cbind, result_list))

    names(keep_df) <- as.character(keep_cols)

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
    # If keep_cols is empty, do a simple unnest
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
