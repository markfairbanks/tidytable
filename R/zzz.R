.onAttach <- function(...) {
  msg <- glue::glue(
    "As of tidytable v0.9.0 dotless versions of functions are exported.
    You can now use `arrange()`/`mutate()`/etc. directly."
  )
  packageStartupMessage(msg)
}
