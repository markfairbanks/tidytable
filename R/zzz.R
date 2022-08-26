.onLoad <- function(...) {
  msg <- glue(
    "As of tidytable v0.9.0, dotless versions of functions are exported.
    You can now use `arrange()`/`mutate()`/etc. directly.
    Note: The `verb.()` versions will remain in the package."
  )
  message(msg)
}
