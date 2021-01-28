
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidytable <img id="logo" src="man/figures/logo.png" align="right" width="17%" height="17%" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/tidytable)](https://cran.r-project.org/package=tidytable)
[![](https://img.shields.io/badge/dev%20-0.5.8-green.svg)](https://github.com/markfairbanks/tidytable)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/last-month/tidytable?color=blue)](https://markfairbanks.github.io/tidytable/)
<!-- badges: end -->

#### Why `tidytable`?

-   `tidyverse`-like syntax with `data.table` speed
-   `rlang` compatibility
-   Includes functions that `dtplyr` is missing, including many `tidyr`
    functions

## Installation

Install the released version from [CRAN](https://CRAN.R-project.org)
with:

``` r
install.packages("tidytable")
```

Or install the development version from [GitHub](https://github.com/)
with:

``` r
# install.packages("devtools")
devtools::install_github("markfairbanks/tidytable")
```

## General syntax

`tidytable` uses `verb.()` syntax to replicate `tidyverse` functions:

``` r
library(tidytable)

test_df <- data.table(x = 1:3, y = 4:6, z = c("a","a","b"))

test_df %>%
  select.(x, y, z) %>%
  filter.(x < 4, y > 1) %>%
  arrange.(x, y) %>%
  mutate.(double_x = x * 2,
          double_y = y * 2)
#> # tidytable [3 × 5]
#>       x     y z     double_x double_y
#>   <int> <int> <chr>    <dbl>    <dbl>
#> 1     1     4 a            2        8
#> 2     2     5 a            4       10
#> 3     3     6 b            6       12
```

A full list of functions can be found
[here](https://markfairbanks.github.io/tidytable/reference/index.html).

## Using “group by”

Group by calls are done from inside any function that has group by
functionality (such as `summarize.()` & `mutate.()`)

-   A single column can be passed with `.by = z`
-   Multiple columns can be passed with `.by = c(y, z)`

``` r
test_df %>%
  summarize.(avg_x = mean(x),
             count = n.(),
             .by = z)
#> # tidytable [2 × 3]
#>   z     avg_x count
#>   <chr> <dbl> <int>
#> 1 a       1.5     2
#> 2 b       3       1
```

### `.by` vs. `group_by()`

A key difference between `tidytable`/`data.table` & `dplyr` is that
`dplyr` can have multiple functions operate “by group” with a single
`group_by()` call.

We’ll start with an example `dplyr` pipe chain that utilizes
`group_by()` and then rewrite it in `tidytable`. The goal is to grab the
first two rows of each group using `slice()`, then add a row number
column using `mutate()`:

``` r
library(dplyr)

test_df <- tibble(x = c("a", "a", "a", "b", "b"))

test_df %>%
  group_by(x) %>%
  slice(1:2) %>%
  mutate(group_row_num = row_number()) %>%
  ungroup()
#> # A tibble: 4 x 2
#>   x     group_row_num
#>   <chr>         <int>
#> 1 a                 1
#> 2 a                 2
#> 3 b                 1
#> 4 b                 2
```

In this case both `slice()` and `mutate()` will operate “by group”. This
happens until you call `ungroup()` at the end of the chain.

However `data.table` doesn’t “remember” groups between function calls.
So in `tidytable` you need to call `.by` in each function you want to
operate “by group”, and you don’t need to call `ungroup()` at the end:

``` r
library(tidytable)

test_df %>%
  slice.(1:2, .by = x) %>%
  mutate.(group_row_num = row_number.(), .by = x)
#> # tidytable [4 × 2]
#>   x     group_row_num
#>   <chr>         <int>
#> 1 a                 1
#> 2 a                 2
#> 3 b                 1
#> 4 b                 2
```

## `tidyselect` support

`tidytable` allows you to select/drop columns just like you would in the
tidyverse by utilizing the [`tidyselect`](https://tidyselect.r-lib.org)
package in the background.

Normal selection can be mixed with all `tidyselect` helpers:
`everything()`, `starts_with()`, `ends_with()`, `any_of()`, `where()`,
etc.

``` r
test_df <- data.table(
  a = 1:3,
  b1 = 4:6,
  b2 = 7:9,
  c = c("a","a","b")
)

test_df %>%
  select.(a, starts_with("b"))
#> # tidytable [3 × 3]
#>       a    b1    b2
#>   <int> <int> <int>
#> 1     1     4     7
#> 2     2     5     8
#> 3     3     6     9
```

To drop columns use a `-` sign:

``` r
test_df %>%
  select.(-a, -starts_with("b"))
#> # tidytable [3 × 1]
#>   c    
#>   <chr>
#> 1 a    
#> 2 a    
#> 3 b
```

These same ideas can be used whenever selecting columns in `tidytable`
functions - for example when using `count.()`, `drop_na.()`,
`mutate_across.()`, `pivot_longer.()`, etc.

A full overview of selection options can be found
[here](https://tidyselect.r-lib.org/reference/language.html).

### Using tidyselect in `.by`

`tidyselect` helpers also work when using `.by`:

``` r
test_df <- data.table(
  a = 1:3,
  b = 4:6,
  c = c("a","a","b"),
  d = c("a","a","b")
)

test_df %>%
  summarize.(avg_b = mean(b), .by = where(is.character))
#> # tidytable [2 × 3]
#>   c     d     avg_b
#>   <chr> <chr> <dbl>
#> 1 a     a       4.5
#> 2 b     b       6
```

## `rlang` compatibility

`rlang` can be used to write custom functions with `tidytable`
functions. The embracing shortcut `{{ }}` works, or you can use
`enquo()` with `!!` if you prefer.

``` r
df <- data.table(x = c(1,1,1), y = c(1,1,1), z = c("a","a","b"))

add_one <- function(data, add_col) {
  data %>%
    mutate.(new_col = {{ add_col }} + 1)
}

df %>%
  add_one(x)
#> # tidytable [3 × 4]
#>       x     y z     new_col
#>   <dbl> <dbl> <chr>   <dbl>
#> 1     1     1 a           2
#> 2     1     1 a           2
#> 3     1     1 b           2
```

## Auto-conversion

All `tidytable` functions automatically convert `data.frame` and
`tibble` inputs to a `data.table`:

``` r
library(dplyr)
library(data.table)

test_df <- tibble(x = 1:3, y = 4:6, z = c("a","a","b"))

test_df %>%
  mutate.(double_x = x * 2) %>%
  is.data.table()
#> [1] TRUE
```

## `dt()` helper

The `dt()` function makes regular `data.table` syntax pipeable, so you
can easily mix `tidytable` syntax with `data.table` syntax:

``` r
df <- data.table(x = 1:3, y = 4:6, z = c("a", "a", "b"))

df %>%
  dt(, list(x, y, z)) %>%
  dt(x < 4 & y > 1) %>%
  dt(order(x, y)) %>%
  dt(, double_x := x * 2) %>%
  dt(, list(avg_x = mean(x)), by = z)
#> # tidytable [2 × 2]
#>   z     avg_x
#>   <chr> <dbl>
#> 1 a       1.5
#> 2 b       3
```

## Speed Comparisons

For those interested in performance, speed comparisons can be found
[here](https://markfairbanks.github.io/tidytable/articles/speed_comparisons.html).
