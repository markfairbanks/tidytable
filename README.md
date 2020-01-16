
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidydt v0.2.0 <img src="man/figures/logo.png" align="right" width="15%" height="15%" />

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

The goal of `tidydt` is to be a tidy interface to `data.table`.

`tidydt` is `rlang` compatible. [See examples
here](https://github.com/markfairbanks/tidydt#rlang-compatibility)

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("markfairbanks/tidydt")
```

## tidydt functions

### tidydt helpers

  - `as_dt()`: Safely operate on data.tables/convert data.frames to
    data.tables
  - `%notin%`

### dplyr

##### Core verbs

  - `dt_arrange()`
  - `dt_filter()`
  - `dt_mutate()`, `dt_mutate_if()`, `dt_mutate_at()`, &
    `dt_mutate_all()`
  - `dt_select()`, `dt_select_if()`
  - `dt_summarize()`: Group by specifications called inside. See
    `?dt_summarize`

##### Other dplyr functions

  - `dt_bind_rows()` & `dt_bind_cols()`
  - `dt_case_when()`: See `?dt_case_when()` for syntax (slightly
    different than `dplyr::case_when()`)
  - `dt_count()`
  - `dt_distinct()`
  - `dt_pull()`
  - `dt_rename()`, `dt_rename_if()`, `dt_rename_at()`, &
    `dt_rename_all()`
  - `dt_slice()`
  - `dt_top_n()`
  - Joins: `dt_left_join()`, `dt_inner_join()`, `dt_right_join()`, &
    `dt_full_join()`
  - Select helpers: `dt_starts_with()`, `dt_ends_with()`,
    `dt_contains()`, `dt_everything()`

### tidyr

  - `dt_drop_na()`
  - `dt_fill()`
  - `dt_pivot_longer()` & `dt_pivot_wider()`
  - `dt_replace_na()`
  - `dt_group_nest()` & `dt_unnest()`

### purrr

  - `dt_map()`, `dt_map2()`, `dt_map_*()` variants, & `dt_map2_*()`
    variants

## General syntax

The code chunk below shows the `tidydt` syntax:

``` r
library(tidydt) # Loads data.table and %>%

example_dt <- data.table(x = c(1,2,3), y = c(4,5,6), z = c("a", "a", "b"))

example_dt %>%
  as_dt() %>%
  dt_select(x, y, z) %>%
  dt_filter(x < 4, y > 1) %>%
  dt_arrange(x, y) %>%
  dt_mutate(double_x = x * 2,
            double_y = y * 2) %>%
  dt_mutate_if(is.character, as.factor) %>%
  dt_rename(new_x = x,
            new_y = y)
#>    new_x new_y z double_x double_y
#> 1:     1     4 a        2        8
#> 2:     2     5 a        4       10
#> 3:     3     6 b        6       12
```

Group by calls are done from inside any function that has group by
functionality (e.g. `dt_summarize()` & `dt_mutate()`)

A single bare column can be passed with `by = z`. Multiple columns can
be passed with `by = list(y,
z)`.

``` r
example_dt <- data.table(x = c(1,2,3), y = c(4,5,6), z = c("a", "a", "b"))

example_dt %>%
  dt_summarize(avg_x = mean(x),
               count = .N,
               by = z)
#>    z avg_x count
#> 1: a   1.5     2
#> 2: b   3.0     1
```

## `rlang` compatibility

`rlang` quoting/unquoting can be used to write custom functions with
`tidydt` functions.

Note that quosures are not compatible with `data.table`, so `enexpr()`
must be used instead of
`enquo()`.

##### Custom function with `dt_mutate()`

``` r
example_dt <- data.table(x = c(1,2,3), y = c(4,5,6), z = c("a", "a", "b"))

add_one <- function(.data, new_name, add_col) {
  new_name <- enexpr(new_name)
  add_col <- enexpr(add_col)
  
  .data %>%
    dt_mutate(!!new_name := !!add_col + 1)
}

example_dt %>%
  add_one(x_plus_one, x)
#>    x y z x_plus_one
#> 1: 1 4 a          2
#> 2: 2 5 a          3
#> 3: 3 6 b          4
```

##### Custom function with `dt_summarize()`

``` r
example_df <- data.table(x = 1:10, y = c(rep("a", 6), rep("b", 4)), z = c(rep("a", 6), rep("b", 4)))

find_mean <- function(.data, grouping_cols, col) {
  grouping_cols <- enexpr(grouping_cols)
  col <- enexpr(col)
  
  .data %>%
    dt_summarize(avg = mean(!!col),
                 by = !!grouping_cols)
}

example_df %>%
  find_mean(grouping_cols = list(y, z), col = x)
#>    y z avg
#> 1: a a 3.5
#> 2: b b 8.5
```

### `dt()` helper

The `dt()` function makes regular `data.table` syntax pipeable, so you
can easily mix `tidydt` syntax with `data.table`
syntax:

``` r
example_dt <- data.table(x = c(1,2,3), y = c(4,5,6), z = c("a", "a", "b"))

example_dt %>%
  as_dt() %>%
  dt(, list(x, y, z)) %>%
  dt(x < 4 & y > 1) %>%
  dt(order(x, y)) %>%
  dt(, ':='(double_x = x * 2,
            double_y = y * 2)) %>%
  dt(, list(avg_x = mean(x)), by = z)
#>    z avg_x
#> 1: a   1.5
#> 2: b   3.0
```
