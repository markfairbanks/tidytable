#' Unnest a list-column of vectors into regular columns
#'
#' @description
#' Turns each element of a list-column into a row.
#'
#' @param .df A data.table or data.frame
#' @param col Column to unnest
#' @param values_to Name of column to store values
#' @param indices_to Name of column to store indices
#' @param indices_include Should an index column be included?
#'   Defaults to `TRUE` when `col` has inner names.
#' @param keep_empty Return `NA` for any `NULL` elements of the list column
#' @param names_repair Treatment of duplicate names. See `?vctrs::vec_as_names` for options/details.
#' @param simplify Currently not supported. Errors if not `NULL`.
#' @param ptype Optionally a named list of ptypes declaring the desired output type of each component.
#' @param transform Optionally a named list of transformation functions applied to each component.
#'
#' @export
#'
#' @examples
#' df <- tidytable(
#'   x = 1:3,
#'   y = list(0, 1:3, 4:5)
#' )
#'
#' df %>% unnest_longer(y)
unnest_longer <- function(.df, col, values_to = NULL, indices_to = NULL,
                          indices_include = NULL, keep_empty = FALSE,
                          names_repair = "check_unique", simplify = NULL,
                          ptype = NULL, transform = NULL) {
  UseMethod("unnest_longer")
}

#' @export
unnest_longer.tidytable <- function(.df, col, values_to = NULL, indices_to = NULL,
                                    indices_include = NULL, keep_empty = FALSE,
                                    names_repair = "check_unique", simplify = NULL,
                                    ptype = NULL, transform = NULL) {
  .col <- enquo(col)

  x <- pull(.df, !!.col)

  x_val <- list_drop_empty(x)[[1]]
  if (!is_simple_vector(x_val)) {
    abort("Only vectors are currently supported")
  }

  if (!is.null(simplify)) {
    abort("The simplify argument is not currently supported")
  }

  .col_name <- as_name(.col)
  .values_to <- sym(values_to %||% .col_name)
  .indices_include <- indices_include %||% any(map_lgl(x, ~ any(have_name(.x))))

  if (.indices_include || !is.null(indices_to)) {
    .indices_to <- sym(indices_to %||% paste0(.col_name, "_id"))

    .df <- mutate(.df, !!.indices_to := map(!!.col, ~ names(.x) %||% seq_along(.x)))
  } else {
    .indices_to <- character()
  }

  if (!is.null(values_to)) {
    .df <- rename(.df, !!.values_to := !!.col)
  }

  to_vec <- as.character(c(.values_to, .indices_to))

  out <- unnest(
    .df, all_of(to_vec),
    names_repair = names_repair,
    keep_empty = keep_empty,
    .drop = FALSE
  )

  out <- change_types(out, to_vec, ptype, transform)

  out
}

#' @export
unnest_longer.data.frame <- function(.df, col, values_to = NULL, indices_to = NULL,
                                     indices_include = NULL, keep_empty = FALSE,
                                     names_repair = "check_unique", simplify = NULL,
                                     ptype = NULL, transform = NULL) {
  .df <- as_tidytable(.df)
  unnest_longer(
    .df, col = {{ col }}, values_to = values_to, indices_to = indices_to,
    indices_include = indices_include, keep_empty = keep_empty,
    names_repair = names_repair, simplify = simplify,
    ptype = ptype, transform = transform
  )
}

