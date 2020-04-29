# Suppress R CMD check note
#' @import data.table
#' @import tidyselect
#' @importFrom methods as
#' @importFrom rlang abort as_function caller_env enexpr enexprs expr expr_text eval_tidy have_name
#' @importFrom rlang is_formula is_named is_null missing_arg parse_expr seq2 set_names squash sym syms
#' @importFrom rlang `%||%`
#' @importFrom stats as.formula na.omit setNames
#' @importFrom utils capture.output head tail getFromNamespace
#' @importFrom vctrs vec_assert vec_cast vec_ptype_common
NULL

globalVariables(c("data", ".","..select_vars", ".count", "na_index", ".new_col",
                  "..all_names", "..final_order_i", "..rows", "name", "value",
                  "..all_cols", "..select_cols", "..keep_names", "..unite_cols",
                  "..keep_cols"))

#' @docType import
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
