.onAttach <- function(...) {
  .msg <- glue::glue(
    "As of tidytable v0.9.0 dotless versions of functions are exported.
    You can now use `arrange()`/`mutate()`/etc. directly."
  )
  packageStartupMessage(.msg)
  if (vctrs::vec_in("dplyr", .packages())) {
    .fn <- "dplyr"
    .load_msg <- glue::glue(
      "Warning: tidytable was loaded after {.fn}.
      This can lead to most {.fn} functions being overwritten by tidytable functions."
    )
    packageStartupMessage(.load_msg)
  }
  if (vctrs::vec_in("tidyr", .packages())) {
    .fn <- "tidyr"
    .load_msg <- glue::glue(
      "Warning: tidytable was loaded after {.fn}.
      This can lead to most {.fn} functions being overwritten by tidytable functions."
    )
    packageStartupMessage(.load_msg)
  }
}
