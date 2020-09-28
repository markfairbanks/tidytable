
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidytable <img src="man/figures/logo.png" align="right" width="17%" height="17%" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/tidytable)](https://cran.r-project.org/package=tidytable)
[![](https://img.shields.io/badge/dev%20-0.5.6-green.svg)](https://github.com/markfairbanks/tidytable)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/last-month/tidytable?color=grey)](https://markfairbanks.github.io/tidytable/)
<!-- badges: end -->

#### Why `tidytable`?

  - `tidyverse`-like syntax with `data.table` speed
  - `rlang` compatibility
  - Includes functions that `dtplyr` is missing, including many `tidyr`
    functions

Note: `tidytable` functions do not use `data.table`’s
modify-by-reference, and instead use the copy-on-modify principles
followed by the `tidyverse` and base R.

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

test_df <- data.table(x = c(1,2,3), y = c(4,5,6), z = c("a","a","b"))

test_df %>%
  select.(x, y, z) %>%
  filter.(x < 4, y > 1) %>%
  arrange.(x, y) %>%
  mutate.(double_x = x * 2,
          double_y = y * 2)
#> # tidytable [3 × 5]
#>       x     y z     double_x double_y
#>   <dbl> <dbl> <chr>    <dbl>    <dbl>
#> 1     1     4 a            2        8
#> 2     2     5 a            4       10
#> 3     3     6 b            6       12
```

A full list of functions can be found
[here](https://markfairbanks.github.io/tidytable/reference/index.html).

## Using “group by”

Group by calls are done from inside any function that has group by
functionality (such as `summarize.()` & `mutate.()`)

  - A single column can be passed with `.by = z`
  - Multiple columns can be passed with `.by = c(y, z)`
  - [`tidyselect`](https://tidyselect.r-lib.org/reference/language.html)
    can also be used, including using predicates:
      - Single predicate: `.by = where(is.character)`
      - Multiple predicates: `.by = c(where(is.character),
        where(is.factor))`
      - A combination of predicates and column names: `.by =
        c(where(is.character), y)`

<!-- end list -->

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

## `tidyselect` support

`tidytable` allows you to select/drop columns just like you would in the
tidyverse.

Normal selection can be mixed with:

  - Predicates: `where(is.numeric)`, `where(is.character)`, etc.
  - Select helpers: `everything()`, `starts_with()`, `ends_with()`,
    `contains()`, `any_of()`, etc.

<!-- end list -->

``` r
test_df <- data.table(a = c(1,2,3),
                      b = c(4,5,6),
                      c = c("a","a","b"),
                      d = c("a","b","c"))

test_df %>%
  select.(a, where(is.character))
#> # tidytable [3 × 3]
#>       a c     d    
#>   <dbl> <chr> <chr>
#> 1     1 a     a    
#> 2     2 a     b    
#> 3     3 b     c
```

To drop columns use a `-` sign:

``` r
test_df %>%
  select.(-a, -where(is.character))
#> # tidytable [3 × 1]
#>       b
#>   <dbl>
#> 1     4
#> 2     5
#> 3     6
```

These same ideas can be used whenever selecting columns in `tidytable`
functions - for example when using `count.()`, `drop_na.()`,
`mutate_across.()`, `pivot_longer.()`, etc.

A full overview of selection options can be found
[here](https://tidyselect.r-lib.org/reference/language.html).

## `rlang` compatibility

`rlang` can be used to write custom functions with `tidytable`
functions:

##### Custom function with `mutate.()`

``` r
df <- data.table(x = c(1,1,1), y = c(1,1,1), z = c("a","a","b"))

# Using enquo() with !!
add_one <- function(data, add_col) {
  
  add_col <- enquo(add_col)
  
  data %>%
    mutate.(new_col = !!add_col + 1)
}

# Using the {{ }} shortcut
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

##### Custom function with `summarize.()`

``` r
df <- data.table(x = 1:10, y = c(rep("a", 6), rep("b", 4)), z = c(rep("a", 6), rep("b", 4)))

find_mean <- function(data, grouping_cols, col) {
  data %>%
    summarize.(avg = mean({{ col }}),
               .by = {{ grouping_cols }})
}

df %>%
  find_mean(grouping_cols = c(y, z), col = x)
#> # tidytable [2 × 3]
#>   y     z       avg
#>   <chr> <chr> <dbl>
#> 1 a     a       3.5
#> 2 b     b       8.5
```

## Auto-conversion

All `tidytable` functions automatically convert `data.frame` and
`tibble` inputs to a `data.table`:

``` r
library(dplyr)
library(data.table)

test_df <- tibble(x = c(1,2,3), y = c(4,5,6), z = c("a","a","b"))

test_df %>%
  mutate.(double_x = x * 2) %>%
  is.data.table()
#> [1] TRUE
```

## `dt()` helper

The `dt()` function makes regular `data.table` syntax pipeable, so you
can easily mix `tidytable` syntax with `data.table` syntax:

``` r
df <- data.table(x = c(1,2,3), y = c(4,5,6), z = c("a", "a", "b"))

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
