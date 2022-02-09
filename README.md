
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidytable <img id="logo" src="man/figures/logo.png" align="right" width="17%" height="17%" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/tidytable)](https://cran.r-project.org/package=tidytable)
[![downloads](http://cranlogs.r-pkg.org/badges/grand-total/tidytable?color=blue)](https://r-pkg.org/pkg/tidytable)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/last-month/tidytable?color=blue)](https://markfairbanks.github.io/tidytable/)
[![R-CMD-check](https://github.com/markfairbanks/tidytable/workflows/R-CMD-check/badge.svg)](https://github.com/markfairbanks/tidytable/actions)
<!-- badges: end -->

#### Why `tidytable`?

-   `tidyverse`-like syntax built on top of the fast `data.table`
    package
-   Compatibility with the tidy evaluation framework
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

df <- data.table(x = 1:3, y = 4:6, z = c("a", "a", "b"))

df %>%
  select.(x, y, z) %>%
  filter.(x < 4, y > 1) %>%
  arrange.(x, y) %>%
  mutate.(double_x = x * 2,
          x_plus_y = x + y)
#> # A tidytable: 3 × 5
#>       x     y z     double_x x_plus_y
#>   <int> <int> <chr>    <dbl>    <int>
#> 1     1     4 a            2        5
#> 2     2     5 a            4        7
#> 3     3     6 b            6        9
```

A full list of functions can be found
[here](https://markfairbanks.github.io/tidytable/reference/index.html).

## Using “group by”

Group by calls are done by using the `.by` argument of any function that
has “by group” functionality.

-   A single column can be passed with `.by = z`
-   Multiple columns can be passed with `.by = c(y, z)`

``` r
df %>%
  summarize.(avg_x = mean(x),
             count = n(),
             .by = z)
#> # A tidytable: 2 × 3
#>   z     avg_x count
#>   <chr> <dbl> <int>
#> 1 a       1.5     2
#> 2 b       3       1
```

### `.by` vs. `group_by()`

`tidytable` follows `data.table` semantics where `.by` must be called
each time you want a function to operate “by group”.

Below is some example `tidytable` code that utilizes `.by` that we’ll
then compare to its `dplyr` equivalent. The goal is to grab the first
two rows of each group using `slice.()`, then add a group row number
column using `mutate.()`:

``` r
library(tidytable)

df <- data.table(x = c("a", "a", "a", "b", "b"))

df %>%
  slice.(1:2, .by = x) %>%
  mutate.(group_row_num = row_number(), .by = x)
#> # A tidytable: 4 × 2
#>   x     group_row_num
#>   <chr>         <int>
#> 1 a                 1
#> 2 a                 2
#> 3 b                 1
#> 4 b                 2
```

Note how `.by` is called in both `slice.()` and `mutate.()`.

Compared to a `dplyr` pipe chain that utilizes `group_by()`, where each
function operates “by group” until `ungroup()` is called:

``` r
library(dplyr)

df <- tibble(x = c("a", "a", "a", "b", "b"))

df %>%
  group_by(x) %>%
  slice(1:2) %>%
  mutate(group_row_num = row_number()) %>%
  ungroup()
#> # A tibble: 4 × 2
#>   x     group_row_num
#>   <chr>         <int>
#> 1 a                 1
#> 2 a                 2
#> 3 b                 1
#> 4 b                 2
```

Note that the `ungroup()` call is unnecessary in `tidytable`.

## tidyselect support

`tidytable` allows you to select/drop columns just like you would in the
tidyverse by utilizing the [`tidyselect`](https://tidyselect.r-lib.org)
package in the background.

Normal selection can be mixed with all `tidyselect` helpers:
`everything()`, `starts_with()`, `ends_with()`, `any_of()`, `where()`,
etc.

``` r
df <- data.table(
  a = 1:3,
  b1 = 4:6,
  b2 = 7:9,
  c = c("a", "a", "b")
)

df %>%
  select.(a, starts_with("b"))
#> # A tidytable: 3 × 3
#>       a    b1    b2
#>   <int> <int> <int>
#> 1     1     4     7
#> 2     2     5     8
#> 3     3     6     9
```

To drop columns use a `-` sign:

``` r
df %>%
  select.(-a, -starts_with("b"))
#> # A tidytable: 3 × 1
#>   c    
#>   <chr>
#> 1 a    
#> 2 a    
#> 3 b
```

These same ideas can be used whenever selecting columns in `tidytable`
functions - for example when using `count.()`, `drop_na.()`,
`across.()`, `pivot_longer.()`, etc.

A full overview of selection options can be found
[here](https://tidyselect.r-lib.org/reference/language.html).

### Using tidyselect in `.by`

`tidyselect` helpers also work when using `.by`:

``` r
df <- data.table(
  a = 1:3,
  b = c("a", "a", "b"),
  c = c("a", "a", "b")
)

df %>%
  summarize.(avg_a = mean(a), .by = where(is.character))
#> # A tidytable: 2 × 3
#>   b     c     avg_a
#>   <chr> <chr> <dbl>
#> 1 a     a       1.5
#> 2 b     b       3
```

## Tidy evaluation compatibility

Tidy evaluation can be used to write custom functions with `tidytable`
functions. The embracing shortcut `{{ }}` works, or you can use
`enquo()` with `!!` if you prefer:

``` r
df <- data.table(x = c(1, 1, 1), y = c(1, 1, 1), z = c("a", "a", "b"))

add_one <- function(data, add_col) {
  data %>%
    mutate.(new_col = {{ add_col }} + 1)
}

df %>%
  add_one(x)
#> # A tidytable: 3 × 4
#>       x     y z     new_col
#>   <dbl> <dbl> <chr>   <dbl>
#> 1     1     1 a           2
#> 2     1     1 a           2
#> 3     1     1 b           2
```

The `.data` and `.env` pronouns also work within `tidytable` functions:

``` r
var <- 10

df %>%
  mutate.(new_col = .data$x + .env$var)
#> # A tidytable: 3 × 4
#>       x     y z     new_col
#>   <dbl> <dbl> <chr>   <dbl>
#> 1     1     1 a          11
#> 2     1     1 a          11
#> 3     1     1 b          11
```

A full overview of tidy evaluation can be found
[here](https://rlang.r-lib.org/reference/topic-data-mask.html).

## `dt()` helper

The `dt()` function makes regular `data.table` syntax pipeable, so you
can easily mix `tidytable` syntax with `data.table` syntax:

``` r
df <- data.table(x = 1:3, y = 4:6, z = c("a", "a", "b"))

df %>%
  dt(, .(x, y, z)) %>%
  dt(x < 4 & y > 1) %>%
  dt(order(x, y)) %>%
  dt(, double_x := x * 2) %>%
  dt(, .(avg_x = mean(x)), by = z)
#> # A tidytable: 2 × 2
#>   z     avg_x
#>   <chr> <dbl>
#> 1 a       1.5
#> 2 b       3
```

## Speed Comparisons

For those interested in performance, speed comparisons can be found
[here](https://markfairbanks.github.io/tidytable/articles/speed_comparisons.html).
