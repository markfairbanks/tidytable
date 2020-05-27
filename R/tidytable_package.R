# Suppress R CMD check note
#' @import data.table
#' @import tidyselect
#' @importFrom lifecycle deprecate_soft
#' @importFrom methods as
#' @importFrom rlang abort as_function caller_env call2 enexpr enexprs enquo enquos expr
#' @importFrom rlang expr_text eval_tidy have_name is_formula is_named is_null missing_arg
#' @importFrom rlang quo quo_is_null quo_squash quo_text seq2 set_names squash sym syms
#' @importFrom rlang `%|%` `%||%`
#' @importFrom stats as.formula na.omit setNames
#' @importFrom utils capture.output head tail
#' @importFrom vctrs vec_assert vec_as_names vec_as_names_legacy vec_cast vec_ptype_common
#' @importFrom vctrs vec_recycle vec_size vec_unique
NULL

globalVariables(c("data", ".","..select_vars", ".count", "na_index", ".new_col",
                  "..all_names", "..final_order_i", "..rows", "name", "value",
                  "..all_cols", "..select_cols", "..keep_names", "..unite_cols",
                  "..keep_cols", ".env"))

#' @docType import

## Reexports ------------------------

## tidyselect ------------------------
#' @export
tidyselect::starts_with

#' @export
tidyselect::contains

#' @export
tidyselect::ends_with

#' @export
tidyselect::everything

#' @export
tidyselect::any_of

#' @export
tidyselect::all_of

#' @export
tidyselect::matches

#' @export
tidyselect::num_range

#' @export
tidyselect::last_col

## data.table ------------------------
#' @export
data.table::data.table

#' @export
data.table::setDTthreads

## rlang ------------------------
#' @export
rlang::enquo

#' @export
rlang::enquos
