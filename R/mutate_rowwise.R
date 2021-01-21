#' Add/modify columns by row
#'
#' @description
#' Allows you to mutate "by row". this is most useful when a vectorized function doesn't exist.
#'
#' @param .df A data.table or data.frame
#' @param ... Columns to add/modify
#'
#' @export
#'
#' @examples
#' test_df <- data.table(x = runif(6), y = runif(6), z = runif(6))
#'
#' # Compute the mean of x, y, z in each row
#' test_df %>%
#'   mutate_rowwise.(row_mean = mean(c(x, y, z)))
#'
#' # Use c_across.() to more easily select many variables
#' test_df %>%
#'   mutate_rowwise.(row_mean = mean(c_across.(x:z)))
mutate_rowwise. <- function(.df, ...) {
  UseMethod("mutate_rowwise.")
}

#' @export
mutate_rowwise..data.frame <- function(.df, ...) {

  .df <- as_tidytable(.df)

  dots <- enquos(...)

  if (length(dots) == 0) return(.df)

  if (any(names(dots) %in% names(.df))) .df <- copy(.df)
  else .df <- shallow(.df)

  data_env <- env(quo_get_env(dots[[1]]), .df = .df)

  dots_text <- map_chr.(dots, quo_text)
  use_across <- str_detect.(dots_text, "c_across.(", fixed = TRUE)

  if (any(use_across)) {

    cols <- extract_cols(dots_text[use_across])

    selected <- map_chr.(cols, ~ get_selected(.df, .x))

    dots_text[use_across] <- str_replace_all.(
      dots_text[use_across],
      glue("c_across.{cols}"),
      glue("vctrs::vec_c(!!!c({selected}))"),
      fixed = TRUE
    )

    dots[use_across] <- map.(which(use_across), ~ quo_set_expr(dots[[.x]], parse_expr(dots_text[[.x]])))
  }

  .df[, .rowwise_id := .I]

  eval_quo(
    .df[, ':='(!!!dots), by = .rowwise_id],
    new_data_mask(data_env), env = caller_env()
  )

  .df[, .rowwise_id := NULL][]
}

# extract the inside of each c_across call
extract_cols <- function(x) {

  patterns <- c(
    "\\((?>[^()]|(?R))*\\)",
    "c_across\\.(?:.(?!c_across\\.))+",
    "\\((?>[^()]|(?R))*\\)"
  )

  for (pattern in patterns) x <- unlist(str_extract_all.(x, pattern, perl = TRUE))

  vec_unique(x)
}

get_selected <- function(.df, cols) {
  cols <- if (cols == "()") "everything()" else cols

  toString(glue("`{select_vec_chr(.df, !!parse_expr(cols))}`"))
}

globalVariables(".rowwise_id")
