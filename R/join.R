#' Join two data.tables together
#'
#' @description Join two data.tables together
#'
#' @param x A data.frame or data.table
#' @param y A data.frame or data.table
#' @param by A character vector of variables to join by. If NULL, the default, the join will do a natural join, using all variables with common names across the two tables.
#' @param suffix Append created for duplicated column names when using `full_join()`
#' @param ... Other parameters passed on to methods
#' @param keep Should the join keys from both `x` and `y` be preserved in the output?
#'
#' @export
#'
#' @examples
#' df1 <- data.table(x = c("a", "a", "b", "c"), y = 1:4)
#' df2 <- data.table(x = c("a", "b"), z = 5:6)
#'
#' df1 %>% left_join(df2)
#' df1 %>% inner_join(df2)
#' df1 %>% right_join(df2)
#' df1 %>% full_join(df2)
#' df1 %>% anti_join(df2)
left_join <- function(x, y, by = NULL, suffix = c(".x", ".y"), ..., keep = FALSE) {
  c(x, y, on, selection) %<-% join_prep(x, y, by, keep, suffix, "left")

  if (length(on) == 0) {
    deprecate_join_by_character()
    out <- cross_join(x, y)
  } else if (keep) {
    out <- dt(y, x, !!selection, on = on, allow.cartesian = TRUE)
  } else {
    out <- y[x, on = on, allow.cartesian = TRUE]

    out <- set_col_order(out, selection)
  }

  tidytable_restore(out, x)
}

#' @export
#' @rdname left_join
right_join <- function(x, y, by = NULL, suffix = c(".x", ".y"), ..., keep = FALSE) {
  c(x, y, on, selection) %<-% join_prep(x, y, by, keep, suffix, "right")

  if (length(on) == 0) {
    deprecate_join_by_character()
    out <- cross_join(x, y)
  } else if (keep) {
    out <- dt(x, y, !!selection, on = on, allow.cartesian = TRUE)
  } else {
    out <- x[y, on = on, allow.cartesian = TRUE]
  }

  tidytable_restore(out, x)
}

#' @export
#' @rdname left_join
inner_join <- function(x, y, by = NULL, suffix = c(".x", ".y"), ..., keep = FALSE) {
  c(x, y, on, selection) %<-% join_prep(x, y, by, keep, suffix, "inner")

  if (length(on) == 0) {
    deprecate_join_by_character()
    out <- cross_join(x, y)
  } else if (keep) {
    out <- dt(x, y, !!selection, on = on, allow.cartesian = TRUE, nomatch = 0)
  } else {
    out <- x[y, on = on, allow.cartesian = TRUE, nomatch = 0]
  }

  tidytable_restore(out, x)
}

#' @export
#' @rdname left_join
full_join <- function(x, y, by = NULL, suffix = c(".x", ".y"), ..., keep = FALSE) {
  if (!is.data.frame(x) | !is.data.frame(y)) stop("x & y must be a data.frame or data.table")
  if (!is_tidytable(x)) x <- as_tidytable(x)
  if (!is_tidytable(y)) y <- as_tidytable(y)

  if (length(by) == 0 && !is.null(by)) {
    deprecate_join_by_character()
    out <- cross_join(x, y)
  } else if (!keep) {
    out <- join_mold(
      x, y, by = by, suffix = suffix,
      all_x = TRUE, all_y = TRUE
    )

    col_order <- suffix_join_names(names(x), names(y), suffix, keep, get_bys(x, y, by), "full")

    out <- set_col_order(out, col_order)
  } else {
    bys <- get_bys(x, y, by)
    by_x <- bys$x
    by_y <- bys$y

    unique_keys_df <- select(x, any_of(by_x)) %>%
      set_names(by_y) %>%
      bind_rows(
        select(y, any_of(by_y))
      ) %>%
      distinct()

    step_df <- right_join(y, unique_keys_df, keep = TRUE, suffix = c("__temp__", ""))

    drop_cols <- by_y[by_x != by_y]
    if (length(by_y[by_x == by_y]) > 0) {
      drop_cols <- c(drop_cols, paste0(by_y[by_x == by_y], suffix[[2]]))
    }

    out <- right_join(x, step_df, by = by, suffix = suffix, keep = TRUE)
    out <- dt_j(out, (drop_cols) := NULL)
    out <- rename_with(out, ~ temp_names_fix(.x, by_x, suffix[[2]]), ends_with("__temp__"))
  }

  tidytable_restore(out, x)
}

temp_names_fix <- function(names, by_x, y_suffix) {
  new_names <- str_replace(names, "__temp__", "")

  map_chr(new_names, function(.x) if (.x %in% by_x) paste0(.x, y_suffix) else .x)
}

#' @export
#' @rdname left_join
anti_join <- function(x, y, by = NULL) {
  c(x, y, on, selection) %<-% join_prep(x, y, by, keep = FALSE, suffix = NULL, "anti")

  if (length(on) == 0) {
    out <- vec_ptype(x)
  } else {
    out <- x[!y, on = on, allow.cartesian = TRUE]
  }

  tidytable_restore(out, x)
}

#' @export
#' @rdname left_join
semi_join <- function(x, y, by = NULL) {
  c(x, y, on, selection) %<-% join_prep(x, y, by, keep = FALSE, suffix = NULL, "semi")

  if (length(on) == 0) {
    out <- x
  } else {
    out <- fsetdiff(x, x[!y, on = on], all = TRUE)
  }

  tidytable_restore(out, x)
}

get_bys <- function(x, y, by = NULL) {
  if (length(by) == 0 && !is.null(by)) {
    return(character())
  }

  x_names <- names(x)
  y_names <- names(y)

  if (is.null(by)) {
    by_x <- intersect(x_names, y_names)
    by_y <- by_x
    if (length(by_x) == 0) {
      abort("`by` must be supplied when `x` and `y` have no common variables.")
    }
  } else {
    by_x <- names(by)
    by_y <- unname(by)
    if (is.null(by_x)) {
      by_x <- by_y
    }
  }

  by_x[by_x == ""] <- by_y[by_x == ""]

  if (any(by_x %notin% x_names)) abort("by.x columns not in x")
  if (any(by_y %notin% y_names)) abort("by.y columns not in y")

  list(x = by_x, y = by_y)
}

join_prep <- function(x, y, by, keep, suffix, type) {
  if (!is.data.frame(x) | !is.data.frame(y)) {
    abort("x & y must be a data.frame or data.table")
  }
  if (!is_tidytable(x)) x <- as_tidytable(x)
  if (!is_tidytable(y)) y <- as_tidytable(y)

  by <- get_bys(x, y, by)

  if (length(by) == 0) {
    # Allow cross joins
    return(list(x, y, on = character(), selection = character()))
  }

  x_names <- names(x)
  y_names <- names(y)

  if (!keep) {
    y_names <- setdiff(y_names, by$y)
    suffix_names <- intersect(setdiff(x_names, by$x), y_names)
  } else {
    suffix_names <- intersect(x_names, y_names)
  }

  if (length(suffix_names) > 0) {
    x <- set_col_names(x, paste0(suffix_names, suffix[[1]]), suffix_names)
    y <- set_col_names(y, paste0(suffix_names, suffix[[2]]), suffix_names)

    x_names <- names(x)
    y_names <- names(y)

    by_x_suffix <- by$x %in% suffix_names
    if (any(by_x_suffix)) {
      by_y_suffix <- by$y %in% suffix_names
      by$x[by_x_suffix] <- paste0(by$x[by_x_suffix], suffix[[1]])
      by$y[by_y_suffix] <- paste0(by$y[by_y_suffix], suffix[[2]])
    }

    if (!keep) {
      y_names <- setdiff(y_names, by$y)
    }
  }

  if (type == "left") {
    on <- by$x
    names(on) <- by$y
  } else {
    on <- by$y
    names(on) <- by$x
  }

  if (keep) {
    if (type == "left") {
      x_prefix <- "i."
      y_prefix <- "x."
    } else {
      x_prefix <- "x."
      y_prefix <- "i."
    }
    selection <- c(paste0(x_prefix, x_names), paste0(y_prefix, y_names))
    names(selection) <- c(x_names, y_names)
    selection <- call2(".", !!!syms(selection))
  } else {
    if (type == "left") {
      # Rename y's by cols before join
      # https://github.com/markfairbanks/tidytable/issues/625
      y <- set_col_names(y, by$x, by$y)
      on <- by$x
      # For use in `left_join` for column order
      # x_names contains by cols and x cols
      # y_names only contains new cols
      selection <- c(x_names, y_names)
    } else {
      selection <- NULL
    }
  }

  list(x, y, on, selection)
}

globalVariables(c("on", "selection"))

join_mold <- function(x, y, by = NULL, suffix = c(".x", ".y"), all_x, all_y) {
  if (!is.data.frame(x) | !is.data.frame(y)) stop("x & y must be a data.frame or data.table")
  if (!is_tidytable(x)) x <- as_tidytable(x)
  if (!is_tidytable(y)) y <- as_tidytable(y)

  by <- get_bys(x, y, by)

  out <- merge(
    x = x, y = y, by.x = by$x, by.y = by$y, suffixes = suffix,
    all.x = all_x, all.y = all_y, allow.cartesian = TRUE, sort = FALSE
  )

  remove_key(out)
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

deprecate_join_by_character <- function(env = caller_env(), user_env = caller_env(2)) {
  deprecate_soft(
    when = "0.9.2",
    what = I("Using `by = character()` to perform a cross join"),
    with = "cross_join()",
    env = env,
    user_env = user_env
  )
}

#' Nest join
#'
#' @description
#' Join the data from y as a list column onto x.
#'
#' @inheritParams left_join
#' @param name The name of the list-column created by the join. If `NULL` the name of `y` is used.
#'
#' @export
#'
#' @examples
#' df1 <- tidytable(x = 1:3)
#' df2 <- tidytable(x = c(2, 3, 3), y = c("a", "b", "c"))
#'
#' out <- nest_join(df1, df2)
#' out
#' out$df2
nest_join <- function(x, y, by = NULL, keep = FALSE, name = NULL, ...) {
  if (is.null(name)) {
    name <- as_name(enexpr(y))
  }

  by_y <- get_bys(x, y, by)$y

  y <- nest_by(y, all_of(by_y), .key = name)

  null_df <- vec_ptype(pull(y, .env$name)[[1]])

  out <- left_join(x, y, by, keep = keep)

  out <- mutate(out, !!name := replace_na(!!sym(name), list(.env$null_df)))

  tidytable_restore(out, x)
}

#' Cross join
#'
#' @description
#' Cross join each row of `x` to every row in `y`.
#'
#' @inheritParams left_join
#'
#' @export
#'
#' @examples
#' df1 <- tidytable(x = 1:3)
#' df2 <- tidytable(y = 4:6)
#'
#' cross_join(df1, df2)
cross_join <- function(x, y, ..., suffix = c(".x", ".y")) {
  x <- as_tidytable(x)
  y <- as_tidytable(y)

  common_names <- intersect(names(x), names(y))

  if (length(common_names) > 0) {
    new_x_names <- paste0(common_names, suffix[[1]])
    new_y_names <- paste0(common_names, suffix[[2]])
    x <- set_col_names(x, new_x_names, common_names)
    y <- set_col_names(y, new_y_names, common_names)
  }

  expand_grid(x, y, .name_repair = "minimal")
}
