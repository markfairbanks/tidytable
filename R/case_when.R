#' Case When with data.table
#'
#' Does what `dplyr::case_when()` does, with the same syntax, but with
#' `data.table::fifelse()` under the hood
#'
#' @param ... statements of the form: `condition ~ label``, where the label is applied if the condition is met
#'
#' @import data.table
#'
#' @examples
#'
#' x <- rnorm(100)
#' dt_case_when(
#'   x < median(x) ~ "low",
#'   x >= median(x) ~ "high",
#'   is.na(x) ~ "other"
#'   )
#'
#' library(data.table)
#' temp <- data.table(pseudo_id = c(1, 2, 3, 4, 5),
#'                    x = sample(1:5, 5, replace = TRUE))
#' temp[, y := dt_case_when(pseudo_id == 1 ~ x * 1,
#'                          pseudo_id == 2 ~ x * 2,
#'                          pseudo_id == 3 ~ x * 3,
#'                          pseudo_id == 4 ~ x * 4,
#'                          pseudo_id == 5 ~ x * 5)]
#'
#' @export
dt_case_when <- function(...){
  # grab the dots
  dots <- list(...)
  # checking the dots
  .check_dots(dots)
  # extract info from dots
  n <- length(dots)
  conds <- conditions(dots)
  labels <- assigned_label(dots)
  class <- class(labels)

  # make the right NA based on assigned labels
  na_type <-
    switch(class,
           "logical"   = NA,
           "complex"   = NA_complex_,
           "character" = NA_character_,
           "integer"   = NA_integer_,
           NA_real_)

  # create fifelse() call
  calls <- call("fifelse", conds[[n]], labels[[n]], eval(na_type))
  for (i in rev(seq_len(n))[-1]){
    calls <- call("fifelse", conds[[i]], labels[[i]], calls)
  }

  eval(calls, envir = parent.frame())
}


#' fifelse from data.table
#'
#' See \code{data.table::\link[data.table:fifelse]{fifelse()}} for details.
#'
#' @name fifelse
#' @keywords internal
#' @export
#' @importFrom data.table fifelse
NULL

# Helpers -----------------

conditions <- function(list){
  unlist(lapply(list, function(x) x[[2]]))
}
assigned_label <- function(list){
  unlist(lapply(list, function(x) x[[3]]))
}
is_formula <- function(x){
  is.call(x) && x[[1]] == quote(`~`)
}

# Check functions -------------------

.check_dots <- function(dots){
  forms <- all(unlist(lapply(dots, is_formula)))
  if (!forms)
    stop("Not all arguments are formulas", call. = FALSE)
}
