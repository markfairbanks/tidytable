#' Join two data.tables together
#'
#' @description Join two data.tables together
#'
#' @param x A data.frame or data.table
#' @param y A data.frame or data.table
#' @param by A character vector of variables to join by. If NULL, the default, the join will do a natural join, using all variables with common names across the two tables.
#' @param suffix Append created for duplicated column names when using `full_join.()`
#' @param ... Other parameters passed on to methods
#' @param keep Should the join keys from both `x` and `y` be preserved in the output?
#'
#' @return A data.table
#' @export
#'
#' @examples
#' df1 <- data.table(x = c("a", "a", "b"), y = 1:3)
#' df2 <- data.table(x = c("a", "b"), z = 1:2)
#'
#' df1 %>% left_join.(df2)
#' df1 %>% inner_join.(df2)
#' df1 %>% right_join.(df2)
#' df1 %>% full_join.(df2)
#' df1 %>% anti_join.(df2)
left_join. <- function(x, y, by = NULL, suffix = c(".x", ".y"), ..., keep = FALSE) {
  UseMethod("left_join.")
}

#' @export
left_join..default <- function(x, y, by = NULL, suffix = c(".x", ".y"), ..., keep = FALSE) {
  if (!is.data.frame(x) | !is.data.frame(y)) stop("x & y must be a data.frame or data.table")
  if (!is_tidytable(x)) x <- as_tidytable(x)
  if (!is_tidytable(y)) y <- as_tidytable(y)

  by <- get_bys(x, y, by)

  on <- by$x
  names(on) <- by$y

  selection <- join_selection(x, y, by, keep, suffix, "left")

  dt_expr <- call2("[", quo(y), quo(x), selection, on = on, allow.cartesian = TRUE)
  result_df <- eval_tidy(dt_expr)

  tidytable_restore(result_df, x)
}

#' @export
#' @rdname left_join.
right_join. <- function(x, y, by = NULL, suffix = c(".x", ".y"), ..., keep = FALSE) {
  UseMethod("right_join.")
}

#' @export
right_join..default <- function(x, y, by = NULL, suffix = c(".x", ".y"), ..., keep = FALSE) {
  if (!is.data.frame(x) | !is.data.frame(y)) stop("x & y must be a data.frame or data.table")
  if (!is_tidytable(x)) x <- as_tidytable(x)
  if (!is_tidytable(y)) y <- as_tidytable(y)

  by <- get_bys(x, y, by)

  on <- by$y
  names(on) <- by$x

  if (!keep) {
    result_df <- x[y, on = on, allow.cartesian = TRUE]
    names(result_df) <- suffix_join_names(names(x), names(y), suffix, keep, by, "right")
  } else {
    selection <- join_selection(x, y, by, keep, suffix, "right")

    dt_expr <- call2("[", quo(x), quo(y), selection, on = on, allow.cartesian = TRUE)
    result_df <- eval_tidy(dt_expr)
  }

  tidytable_restore(result_df, x)
}

#' @export
#' @rdname left_join.
inner_join. <- function(x, y, by = NULL, suffix = c(".x", ".y"), ..., keep = FALSE) {
  UseMethod("inner_join.")
}

#' @export
inner_join..default <- function(x, y, by = NULL, suffix = c(".x", ".y"), ..., keep = FALSE) {
  if (!is.data.frame(x) | !is.data.frame(y)) stop("x & y must be a data.frame or data.table")
  if (!is_tidytable(x)) x <- as_tidytable(x)
  if (!is_tidytable(y)) y <- as_tidytable(y)

  by <- get_bys(x, y, by)

  on <- by$y
  names(on) <- by$x

  if (!keep) {
    result_df <- x[y, on = on, allow.cartesian = TRUE, nomatch = 0]
    names(result_df) <- suffix_join_names(names(x), names(y), suffix, keep, by, "inner")
  } else {
    selection <- join_selection(x, y, by, keep, suffix, "inner")

    dt_expr <- call2("[", quo(x), quo(y), selection, on = on, allow.cartesian = TRUE, nomatch = 0)
    result_df <- eval_tidy(dt_expr)
  }

  tidytable_restore(result_df, x)
}

#' @export
#' @rdname left_join.
full_join. <- function(x, y, by = NULL, suffix = c(".x", ".y"), ..., keep = FALSE) {
  UseMethod("full_join.")
}

#' @export
full_join..default <- function(x, y, by = NULL, suffix = c(".x", ".y"), ..., keep = FALSE) {
    if (!is.data.frame(x) | !is.data.frame(y)) stop("x & y must be a data.frame or data.table")
    if (!is_tidytable(x)) x <- as_tidytable(x)
    if (!is_tidytable(y)) y <- as_tidytable(y)

    if (!keep) {
      result_df <- join_mold(
        x, y, by = by, suffix = suffix,
        all_x = TRUE, all_y = TRUE
      )

      col_order <- suffix_join_names(names(x), names(y), suffix, keep, get_bys(x, y, by), "full")

      setcolorder(result_df, col_order)
      setkey(result_df, NULL)
    } else {
      bys <- get_bys(x, y, by)
      by_x <- bys$x
      by_y <- bys$y

      unique_keys_df <- x[, ..by_x] %>%
        set_names(by_y) %>%
        bind_rows.(y[, ..by_y]) %>%
        distinct.()

      step_df <- right_join.(y, unique_keys_df, keep = TRUE, suffix = c("__temp__", ""))

      drop_cols <- by_y[by_x != by_y]
      if (length(by_y[by_x == by_y]) > 0) {
        drop_cols <- c(drop_cols, paste0(by_y[by_x == by_y], suffix[[2]]))
      }

      result_df <- right_join.(x, step_df, by = by, suffix = suffix, keep = TRUE)
      result_df <- dt_j(result_df, (drop_cols) := NULL)
      result_df <- rename_with.(result_df, ~ temp_names_fix(.x, by_x, suffix[[2]]), ends_with("__temp__"))
    }

    tidytable_restore(result_df, x)
}

temp_names_fix <- function(names, by_x, y_suffix) {
  new_names <- str_replace.(names, "__temp__", "")

  map_chr.(new_names, ~ if (.x %in% by_x) paste0(.x, y_suffix) else .x)
}

globalVariables(c("..by_x", "..by_y"))

#' @export
#' @rdname left_join.
anti_join. <- function(x, y, by = NULL) {
  UseMethod("anti_join.")
}

#' @export
anti_join..default <- function(x, y, by = NULL) {
  if (!is.data.frame(x) | !is.data.frame(y)) stop("x & y must be a data.frame or data.table")
  if (!is_tidytable(x)) x <- as_tidytable(x)
  if (!is_tidytable(y)) y <- as_tidytable(y)

  by <- get_bys(x, y, by)

  on <- by$y
  names(on) <- by$x

  result_df <- x[!y, on = on, allow.cartesian = TRUE]

  tidytable_restore(result_df, x)
}

#' @export
#' @rdname left_join.
semi_join. <- function(x, y, by = NULL) {
  UseMethod("semi_join.")
}

#' @export
semi_join..default <- function(x, y, by = NULL) {
  if (!is.data.frame(x) | !is.data.frame(y)) stop("x & y must be a data.frame or data.table")
  if (!is_tidytable(x)) x <- as_tidytable(x)
  if (!is_tidytable(y)) y <- as_tidytable(y)

  by <- get_bys(x, y, by)

  on <- by$y
  names(on) <- by$x

  result_df <- fsetdiff(x, x[!y, on = on], all=TRUE)

  tidytable_restore(result_df, x)
}

get_bys <- function(x, y, by = NULL) {
  names_x <- names(x)
  names_y <- names(y)

  if (is.null(by)) {
    by_x <- by_y <- intersect(names_x, names_y)
  } else {
    by_x <- names(by)
    by_y <- unname(by)
    if (is.null(by_x)) {
      by_x <- by_y
    }
  }

  if (any(by_x[by_x != ""] %notin% names_x)) stop("by.x columns not in x")
  if (any(by_y[by_y != ""] %notin% names_y)) stop("by.y columns not in y")

  by_x[by_x == ""] <- by_y[by_x == ""]

  list(x = by_x, y = by_y)
}

join_mold <- function(x, y, by = NULL, suffix = c(".x", ".y"), all_x, all_y) {
  if (!is.data.frame(x) | !is.data.frame(y)) stop("x & y must be a data.frame or data.table")
  if (!is_tidytable(x)) x <- as_tidytable(x)
  if (!is_tidytable(y)) y <- as_tidytable(y)

  by <- get_bys(x, y, by)

  result_df <- merge(
    x = x, y = y, by.x = by$x, by.y = by$y, suffixes = suffix,
    all.x = all_x, all.y = all_y, allow.cartesian = TRUE, sort = FALSE
  )

  setkey(result_df, NULL)

  result_df
}

join_selection <- function(x, y, by, keep, suffix, type = "left") {
  x_names <- names(x)
  y_names <- names(y)

  if (!keep) {
    if (type == "left") {
      y_names <- setdiff(y_names, c(by$x, by$y))
    } else if (type %in% c("inner", "right")) {
      x_names <- setdiff(x_names, c(by$x, by$y))
    }
  }

  result_names <- suffix_join_names(x_names, y_names, suffix, keep, by, type)

  if (type == "left") {
    x_prefix <- "i."
    y_prefix <- "x."
  } else if (type %in% c("inner", "right")) {
    x_prefix <- "x."
    y_prefix <- "i."
  } else {
    abort("Unsupported join type")
  }

  if (length(x_names) > 0) {
    x_names <- paste0(x_prefix, x_names)
  }

  if (length(y_names) > 0) {
    y_names <- paste0(y_prefix, y_names)
  }

  selection <- c(x_names, y_names)

  selection <- syms(selection)
  names(selection) <- result_names

  call2(".", !!!selection)
}

suffix_join_names <- function(x_names, y_names, suffix, keep, by = NULL, type) {
  if (!keep && type != "left") {
    y_names <- y_names[y_names %notin% by$y]
  }
  df_names <- c(x_names, y_names)
  is_x_duplicate <- duplicated(df_names, fromLast = TRUE)
  if (any(is_x_duplicate)) {
    is_y_duplicate <- duplicated(df_names)
    new_names <- df_names
    new_names[is_x_duplicate] <- paste0(new_names[is_x_duplicate], suffix[[1]])
    new_names[is_y_duplicate] <- paste0(new_names[is_y_duplicate], suffix[[2]])
    df_names <- new_names
  }
  df_names
}
