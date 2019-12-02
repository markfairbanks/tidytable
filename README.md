
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gdt

<!-- badges: start -->

<!-- badges: end -->

The goal of `gdt` is to complement `tidyfast` and `maditr` as a tidy
interface to `data.table`.

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
  - `dt_pivot_longer()` & `dt_pivot_wider()`
  - `dt_rename()`
  - `dt_mutate_if()` & `dt_mutate_at()`
  - `%notin%`
  - `dt()`: Pipeable `data.table` call. [See example
    here](https://github.com/mtfairbanks/gdt#dt-helper)

#### tidyfast functions

  - `dt_case_when()`
  - `dt_count()`
  - `dt_separate()`

#### maditr functions

  - `dt_mutate()`
  - `dt_arrange()`
  - `dt_summarize()`
  - `dt_filter()`
  - `dt_left_join()`, `dt_inner_join()`, etc.

## General syntax

The code chunk below shows the `gdt` syntax:

``` r
library(gdt) # Loads data.table, %>%, purrr, and stringr
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
