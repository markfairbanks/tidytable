# Suppress R CMD check note
#' @import data.table
#' @import tidyselect
#' @import vctrs
#' @rawNamespace import(rlang, except = `:=`)
#' @importFrom glue glue glue_data
#' @importFrom pillar glimpse dim_desc tbl_sum
#' @importFrom stats na.omit
#' @importFrom utils head tail type.convert
NULL

#' @docType import

### reexports ------------------------

# tidyselect ------------------------
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

# data.table ------------------------
#' @export
data.table::`%between%`

#' @export
data.table::data.table

#' @export
data.table::`%like%`

#' @export
data.table::getDTthreads

#' @export
data.table::setDTthreads

#' @export
data.table::`%chin%`

# rlang ------------------------
#' @export
rlang::enexpr

#' @export
rlang::enexprs

#' @export
rlang::enquo

#' @export
rlang::enquos

#' @export
rlang::expr

#' @export
rlang::exprs

#' @export
rlang::quo

#' @export
rlang::quos

#' @export
rlang::sym

#' @export
rlang::syms

# pillar ------------------------
#' @export
pillar::glimpse
