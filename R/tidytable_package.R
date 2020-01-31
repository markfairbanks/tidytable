# Suppress R CMD check note
#' @import data.table
#' @importFrom data.table data.table
#' @importFrom data.table melt
#' @importFrom data.table dcast
#' @importFrom rlang abort
#' @importFrom rlang caller_env
#' @importFrom rlang enexpr
#' @importFrom rlang enexprs
#' @importFrom rlang expr
#' @importFrom rlang eval_tidy
#' @importFrom rlang is_formula
#' @importFrom rlang is_named
#' @importFrom rlang parse_expr
#' @importFrom rlang seq2
#' @importFrom rlang sym
#' @importFrom stats as.formula
#' @importFrom stats na.omit
#' @importFrom stats setNames
#' @importFrom utils head
#' @importFrom utils tail
NULL

globalVariables(c("data", ".","..select_vars", ".count", "na_index"))
