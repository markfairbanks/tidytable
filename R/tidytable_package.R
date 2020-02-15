# Suppress R CMD check note
#' @import data.table
#' @importFrom data.table as.data.table copy data.table is.data.table melt dcast
#' @importFrom rlang abort caller_env enexpr enexprs expr eval_tidy
#' @importFrom rlang is_formula is_named parse_expr seq2 sym syms
#' @importFrom stats as.formula na.omit setNames
#' @importFrom utils head tail
NULL

globalVariables(c("data", ".","..select_vars", ".count", "na_index", ".new_col", "..all_names"))
