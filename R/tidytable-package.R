# Suppress R CMD check note
#' @import data.table
#' @import tidyselect
#' @import vctrs
#' @rawNamespace import(rlang, except = `:=`)
#' @importFrom glue glue
#' @importFrom lifecycle deprecated deprecate_soft deprecate_stop deprecate_warn expect_deprecated
#' @importFrom methods as
#' @importFrom stats as.formula na.omit
#' @importFrom tibble glimpse trunc_mat
#' @importFrom utils capture.output head packageName tail type.convert
NULL

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
data.table::`%between%`

#' @export
data.table::data.table

#' @export
data.table::setDTthreads

## rlang ------------------------
#' @export
rlang::enquo

#' @export
rlang::enquos

## tibble ------------------------
#' @export
tibble::glimpse
