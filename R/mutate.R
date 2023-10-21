#' Add/modify/delete columns
#'
#' @description
#' With `mutate()` you can do 3 things:
#' * Add new columns
#' * Modify existing columns
#' * Delete columns
#'
#' @param .df A data.frame or data.table
#' @param ... Columns to add/modify
#' @param .by Columns to group by
#' @param .keep *experimental*:
#'   This is an experimental argument that allows you to control which columns
#'   from `.df` are retained in the output:
#'
#'   * `"all"`, the default, retains all variables.
#'   * `"used"` keeps any variables used to make new variables; it's useful
#'     for checking your work as it displays inputs and outputs side-by-side.
#'   * `"unused"` keeps only existing variables **not** used to make new
#'     variables.
#'   * `"none"`, only keeps grouping keys (like [transmute()]).
#' @param .before,.after Optionally indicate where new columns should be placed.
#' Defaults to the right side of the data frame.
#'
#' @export
#'
#' @examples
#' df <- data.table(
#'   a = 1:3,
#'   b = 4:6,
#'   c = c("a", "a", "b")
#' )
#'
#' df %>%
#'   mutate(double_a = a * 2,
#'          a_plus_b = a + b)
#'
#' df %>%
#'   mutate(double_a = a * 2,
#'          avg_a = mean(a),
#'          .by = c)
#'
#' df %>%
#'   mutate(double_a = a * 2, .keep = "used")
#'
#' df %>%
#'   mutate(double_a = a * 2, .after = a)
mutate <- function(.df, ..., .by = NULL,
                   .keep = c("all", "used", "unused", "none"),
                   .before = NULL, .after = NULL) {
  UseMethod("mutate")
}

#' @export
mutate.tidytable <- function(.df, ..., .by = NULL,
                             .keep = c("all", "used", "unused", "none"),
                             .before = NULL, .after = NULL) {
  .df <- fast_copy(.df)

  .by <- enquo(.by)

  dots <- enquos(...)
  if (length(dots) == 0) return(.df)

  dt_env <- get_dt_env(dots)

  .before <- enquo(.before)
  .after <- enquo(.after)

  needs_relocate <- !quo_is_null(.before) || !quo_is_null(.after)
  if (needs_relocate) {
    original_names <- copy(names(.df))
  }

  if (quo_is_null(.by)) {
    for (i in seq_along(dots)) {
      dots_i <- prep_exprs(dots[i], .df, !!.by, j = TRUE, dt_env)
      if (length(dots_i) == 0) next
      dots_i <- exprs_auto_name(dots_i)
      dots_i <- imap(dots_i, ~ mutate_prep(.df, .x, .y))

      j <- expr(':='(!!!dots_i))

      dt_expr <- call2_j(.df, j)

      .df <- eval_tidy(dt_expr, .df, dt_env)
    }
  } else {
    one_dot <- length(dots) == 1

    if (!one_dot) {
      is_across <- map_lgl(dots[-1], quo_is_call, "across")
      if (any(is_across)) {
        # tidyselect helpers will miss columns made before an across call
        # that is not in the first position
        abort("across() can only be used in the first position of mutate()
              when `.by` is used.")
      }
    }

    dots <- prep_exprs(dots, .df, !!.by, j = TRUE, dt_env)

    .by <- tidyselect_names(.df, !!.by)

    # Check for NULL inputs so columns can be deleted
    # Only delete if the NULL is the last call
    is_null <- vec_detect_missing(dots)
    is_last <- !duplicated(names(dots), fromLast = TRUE)
    needs_removal <- is_null & is_last
    any_null <- any(needs_removal)

    if (any_null) {
      null_dots <- dots[needs_removal]

      dots <- dots[!needs_removal]
    }

    if (length(dots) > 0) {
      dots <- exprs_auto_name(dots)
      dots_names <- names(dots)

      .df <- fast_copy(.df, vec_unique(dots_names))

      needs_sequential <- sequential_check(dots)

      if (one_dot || !needs_sequential) {
        j <- expr(':='(!!!dots))

        dt_expr <- call2_j(.df, j, .by)
      } else {
        assign <- map2(syms(dots_names), dots, ~ call2("=", .x, .y))
        dots_names <- unique(dots_names)
        output <- call2("list", !!!syms(dots_names))
        expr <- call2("{", !!!assign, output)
        j <- call2(":=", call2("c", !!!dots_names), expr)

        dt_expr <- call2_j(.df, j, .by)
      }

      .df <- eval_tidy(dt_expr, .df, dt_env)
    }

    if (any_null) {
      drop_cols <- names(null_dots)
      .df <- dt_j(.df, (drop_cols) := NULL)
    }
  }

  if (needs_relocate) {
    new_names <- setdiff(names(.df), original_names)
    .df <- relocate(.df, !!!syms(new_names), .before = !!.before, .after = !!.after)
  }

  .keep <- arg_match(.keep)
  if (.keep != "all") {
    cols_keep <- get_keep_vars(.df, dots, .by, .keep)
    .df <- select(.df, any_of(cols_keep))
  }

  .df
}

#' @export
mutate.grouped_tt <- function(.df, ..., .by = NULL,
                              .keep = c("all", "used", "unused", "none"),
                              .before = NULL, .after = NULL) {
  .by <- group_vars(.df)
  out <- ungroup(.df)
  out <- mutate(
    out, ...,
    .by = any_of(.by),
    .keep = .keep,
    .before = {{ .before }},
    .after = {{ .after }}
  )
  group_by(out, any_of(.by))
}

#' @export
mutate.data.frame <- function(.df, ..., .by = NULL,
                              .keep = c("all", "used", "unused", "none"),
                              .before = NULL, .after = NULL) {
  .df <- as_tidytable(.df)
  mutate(
    .df, ...,
    .by = {{ .by }},
    .keep = .keep,
    .before = {{ .before }},
    .after = {{ .after }}
  )
}

#' @export
mutate.rowwise_tt <- function(.df, ..., .by = NULL,
                              .keep = c("all", "used", "unused", "none"),
                              .before = NULL, .after = NULL) {
  out <- ungroup(.df)

  out <- mutate(out, .rowwise_id = .I)

  out <- mutate(
    out, ...,
    .by = .rowwise_id,
    .keep = .keep,
    .before = {{ .before }},
    .after = {{ .after }}
  )

  out <- dt_j(out, .rowwise_id := NULL)

  rowwise(out)
}

# vec_recycle() prevents modify-by-reference if the column already exists in the data.table
# Fixes case when user supplies a single value ex. 1, -1, "a"
# !is_null(val) allows for columns to be deleted using mutate(.df, col = NULL)
mutate_prep <- function(data, dot, dot_name) {
  if (dot_name %in% names(data) && !is_null(dot)) {
    dot <- call2("vec_recycle", dot, expr(.N), .ns = "vctrs")
  }
  dot
}

sequential_check <- function(dots) {
  dots_names <- names(dots)

  used_vars <- unique(unlist(map(dots[-1], extract_used)))

  any(dots_names %in% used_vars) || vec_duplicate_any(dots_names)
}

get_keep_vars <- function(df, dots, .by, .keep = "all") {
  if (is_quosure(.by)) {
    dots <- prep_exprs(dots, df, j = TRUE)
    dots <- exprs_auto_name(dots)
    .by <- character()
  }
  df_names <- names(df)
  dots_names <- names(dots)
  used <- unlist(map(dots, extract_used)) %||% character()
  used <- used[used %in% df_names]

  if (.keep == "used") {
    keep <- c(.by, used, dots_names)
  } else if (.keep == "unused") {
    unused <- df_names[df_names %notin% used]
    keep <- c(.by, unused, dots_names)
  } else if (.keep == "none") {
    keep <- c(.by, dots_names)
  }

  keep <- unique(keep)
  df_names[df_names %in% keep] # Preserve column order
}

extract_used <- function(x) {
  if (is.symbol(x)) {
    as.character(x)
  } else {
    unique(unlist(lapply(x[-1], extract_used)))
  }
}
