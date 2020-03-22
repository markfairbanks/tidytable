# Suppress R CMD check note
#' @import data.table
#' @importFrom methods as
#' @importFrom rlang abort as_function caller_env enexpr enexprs expr eval_tidy
#' @importFrom rlang is_formula is_named missing_arg parse_expr seq2 squash sym syms
#' @importFrom stats as.formula na.omit setNames
#' @importFrom utils capture.output head tail
NULL

globalVariables(c("data", ".","..select_vars", ".count", "na_index", ".new_col",
                  "..all_names", "..final_order_i", "..rows", "name", "value",
                  "..all_cols"))
