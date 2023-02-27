.onAttach <- function(...) {
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
