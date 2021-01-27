# Build across calls
across_calls <- function(.fns, .fun, .cols, .names, dots) {

  # Note for .fun = enexpr(.fns)
  # Needed to capture .fun separately for use later, otherwise a bare function call pre-evaluates
  # when .fns is called in is.list(.funs)
  # Ex: data.table mutate_across.(test_df, everything(), between, 1, 3)
  # R will search for data.table's Cbetween instead of the R function
  # This workaround works, but there's probably a better way to deal with this

  if (!is.list(.fns)) {

    if (is_anon_fun(.fns)) .fun <- as_function(.fns)

    call_list <- map.(syms(.cols), ~ call2(.fun, .x, !!!dots))

    .names <- .names %||% "{.col}"

    names(call_list) <- vec_as_names(
      glue(.names, .col = .cols, .fn = "1", col = .cols, fn = "1"),
      repair = "check_unique", quiet = TRUE
    )
  } else {
    names_flag <- have_name(.fns)

    if (!all(names_flag)) names(.fns)[!names_flag] <- seq_len(length(.fns))[!names_flag]

    .fns <- map.(.fns, as_function)

    fn_names <- names(.fns)

    combos <- expand_grid.(.fns = .fns, .cols = .cols)

    .fns <- combos$.fns

    .cols <- combos$.cols

    call_list <- map2.(.fns, syms(.cols), ~ call2(.x, .y, !!!dots))

    fn_names <- vec_rep_each(fn_names, length(.fns)/length(fn_names))

    .names <- .names %||% "{.col}_{.fn}"

    names(call_list) <- vec_as_names(
      glue(.names, .col = .cols, .fn = fn_names, col = .cols, fn = fn_names),
      repair = "check_unique", quiet = TRUE
    )
  }

  call_list
}

# Converts result_expr to text and back to expression to avoid environment issues (#145)
reset_expr <- function(express) {
  parse_expr(quo_text(quo_squash(enquo(
    express
  ))))
}

is_anon_fun <- function(fun) {
  is.null(packageName(get_env(fun))) || is_bare_formula(fun)
}
