
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gdt v0.4.0 <img src="man/figures/logo.png" align="right" width="15%" height="15%" />

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

The goal of `gdt` is to complement
[`tidyfast`](https://github.com/TysonStanley/tidyfast) and
[`maditr`](https://github.com/gdemin/maditr) as a tidy interface to
`data.table`.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mtfairbanks/gdt")
```

## Functions

#### gdt functions

  - `as_dt()`: Safely operate on data.tables without altering the
    original object. Also converts data.frames to data.tables.
  - `dt_mutate()`, `dt_mutate_if()`, `dt_mutate_at()`, &
    `dt_mutate_all()`
  - `dt_select()`, `dt_select_if()`
  - `dt_filter()`
  - `dt_arrange()`
  - `dt_summarize()`
  - `dt_rename()`, `dt_rename_if()`, `dt_rename_at()`, &
    `dt_rename_all()`
  - `dt_slice()`
  - `dt_pull()`
  - `dt_bind_rows()` & `dt_bind_cols()`
  - `dt_distinct()`
  - `dt_drop_na()`
  - `dt_map()`, `dt_map2()`, `dt_map_*()` variants, & `dt_map2_*()`
    variants
  - `%notin%`
  - `dt()`: Pipeable `data.table` call. [See example
    here](https://github.com/mtfairbanks/gdt#dt-helper)

#### tidyfast functions

  - `dt_case_when()`
  - `dt_pivot_longer()` & `dt_pivot_wider()`: `gdt` functions recently
    accepted to `tidyfast`\!
  - `dt_count()` & `dt_uncount()`
  - `dt_separate()`
  - `dt_nest()` & `dt_unnest()`
  - `dt_fill()`

#### maditr functions

  - `dt_left_join()`, `dt_inner_join()`, etc.

## General syntax

The code chunk below shows the `gdt` syntax:

``` r
library(gdt) # Loads data.table and %>%
library(maditr)
library(tidyfast)

example_dt <- data.table(x = c(1,2,3), y = c(4,5,6), z = c("a", "a", "b"))

example_dt %>%
  as_dt() %>% # Safely operate on data.tables/convert data.frames to data.tables
  dt_select(x, y, z) %>%
  dt_filter(x < 4 & y > 1) %>%
  dt_arrange(x, y) %>%
  dt_mutate(double_x = x * 2,
            double_y = y * 2) %>%
  dt_mutate_if(is.character, as.factor) %>%
  dt_summarize(avg_x = mean(x), by = z) %>%
  dt_rename(new_z = z,
            new_avg_x = avg_x) # Rename one or multiple columns
#>    new_z new_avg_x
#> 1:     a       1.5
#> 2:     b       3.0
```

#### `dt()` helper

The `dt()` function makes `data.table` syntax
pipeable:

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
