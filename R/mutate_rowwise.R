#' Add/modify columns by row
#'
#' @description
#' Allows you to mutate "by row". this is most useful when a vectorized function doesn't exist.
#'
#' @param .df A data.table or data.frame
#' @param ... Columns to add/modify
#' @param .keep *experimental*:
#'   This is an experimental argument that allows you to control which columns
#'   from `.df` are retained in the output:
#'
#'   * `"all"`, the default, retains all variables.
#'   * `"used"` keeps any variables used to make new variables; it's useful
#'     for checking your work as it displays inputs and outputs side-by-side.
#'   * `"unused"` keeps only existing variables **not** used to make new
#'     variables.
#'   * `"none"`, only keeps grouping keys (like [transmute.()]).
#' @param .before,.after Optionally indicate where new columns should be placed.
#' Defaults to the right side of the data frame.
#'
#' @export
#'
#' @examples
#' df <- data.table(x = 1:3, y = 1:3 * 2, z = 1:3 * 3)
#'
#' # Compute the mean of x, y, z in each row
#' df %>%
#'   mutate_rowwise.(row_mean = mean(c(x, y, z)))
#'
#' # Use c_across.() to more easily select many variables
#' df %>%
#'   mutate_rowwise.(row_mean = mean(c_across.(x:z)))
mutate_rowwise. <- function(.df, ...,
                            .keep = c("all", "used", "unused", "none"),
                            .before = NULL, .after = NULL) {
  UseMethod("mutate_rowwise.")
}

#' @export
mutate_rowwise..tidytable <- function(.df, ...,
                                      .keep = c("all", "used", "unused", "none"),
                                      .before = NULL, .after = NULL) {
  dots <- enquos(...)
  if (length(dots) == 0) return(.df)

  .df <- mutate.(.df, .rowwise_id = .I)

  .df <- mutate.(.df, !!!dots,
                 .by = .rowwise_id,
                 .keep = .keep,
                 .before = {{ .before }},
                 .after = {{ .after }})

  mutate.(.df, .rowwise_id = NULL)
}

#' @export
mutate_rowwise..data.frame <- function(.df, ...,
                                       .keep = c("all", "used", "unused", "none"),
                                       .before = NULL, .after = NULL) {
  .df <- as_tidytable(.df)
  mutate_rowwise.(.df, ..., .keep = .keep, .before = {{ .before }}, .after = {{ .after }})
}

globalVariables(".rowwise_id")
