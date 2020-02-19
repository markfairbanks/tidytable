
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidytable <img src="man/figures/logo.png" align="right" width="18%" height="18%" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/tidytable)](https://cran.r-project.org/package=tidytable)
[![](https://img.shields.io/badge/devel%20version-0.3.1-blue.svg)](https://github.com/markfairbanks/tidytable)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

The goal of `tidytable` is to be a tidy interface to `data.table`.

`tidytable` is `rlang` compatible. [See examples
here](https://markfairbanks.github.io/tidytable/#rlang-compatibility)

#### Why `tidytable`?

`tidytable` gives the user `tidyverse`-like syntax with `data.table`
speed.

`tidytable` started as a complement to the
[`dtplyr`](https://github.com/tidyverse/dtplyr) package, since `dtplyr`
is missing some of `dplyr`’s functionality and doesn’t cover any `tidyr`
functions. However translations of `dplyr`’s core verbs (mutate,
arrange, etc.) are now included in `tidytable` to allow the user access
to all functions in one place.

As such you can use this as a one stop shop for `data.table`
translations, or you can use it to complement your current `dtplyr`
workflow.

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

## tidytable functions

**Enhanced selection support denoted by ES** [See examples
here](https://markfairbanks.github.io/tidytable/#enhanced-selection)

### tidytable helpers

  - `dt()`: Pipeable `data.table` syntax. [See
    here](https://markfairbanks.github.io/tidytable/#dt-helper)
  - `%notin%`

### dplyr

##### Core verbs

  - `dt_arrange()`
  - `dt_filter()`
  - `dt_mutate()`: `_if()`/`_at()`/`_all()`/`_across()` - **ES**
      - The `_across()` variant is new and can replace both `_if()` and
        `_at()` [See
        here](https://markfairbanks.github.io/tidytable/#new-variant-dt_mutate_across)
  - `dt_select()` - **ES**
  - `dt_summarize()`: Group by specifications called inside. See
    `?dt_summarize`

##### Other dplyr functions

  - `dt_bind_rows()` & `dt_bind_cols()`
  - `dt_case()`: Similar to `dplyr::case_when()`. See `?dt_case()` for
    syntax
  - `dt_count()` - **ES**
  - `dt_distinct()` - **ES**
  - `dt_pull()`
  - `dt_rename()`: `_if()`/`_at()`/`_all()`/`_across()` - **ES**
  - `dt_slice()`: `_head()`/`_tail()`/`_max()`/`_min()`
      - The `slice_*()` variants are like `dt_top_n()`, but are slightly
        easier to use
  - `dt_top_n()`
  - Joins: `dt_left_join()`, `dt_inner_join()`, `dt_right_join()`,
    `dt_full_join()`, & `dt_anti_join()`
  - Select helpers: `dt_starts_with()`, `dt_ends_with()`,
    `dt_contains()`, `dt_everything()`

### tidyr

  - `dt_drop_na()` - **ES**
  - `dt_fill()`: Works on character/factor/logical types
    (`data.table::nafill()` does not) - **ES**
  - `dt_group_split()` - **ES**
  - `dt_pivot_wider()` - **ES** & `dt_pivot_longer()` - **ES**
  - `dt_replace_na()`
  - `dt_group_nest()` - **ES** & `dt_unnest_legacy()`

### purrr

  - `dt_map()`, `dt_map2()`, `dt_map_*()` variants, & `dt_map2_*()`
    variants

## General syntax

The code chunk below shows the `tidytable` syntax:

``` r
library(data.table)
library(tidytable)

example_dt <- data.table(x = c(1,2,3), y = c(4,5,6), z = c("a", "a", "b"))

example_dt %>%
  dt_select(x, y, z) %>%
  dt_filter(x < 4, y > 1) %>%
  dt_arrange(x, y) %>%
  dt_mutate(double_x = x * 2,
            double_y = y * 2) %>%
  dt_mutate_across(is.character, as.factor) %>%
  dt_rename(new_x = x,
            new_y = y)
#>    new_x new_y z double_x double_y
#> 1:     1     4 a        2        8
#> 2:     2     5 a        4       10
#> 3:     3     6 b        6       12
```

#### Using “group by”

Group by calls are done from inside any function that has group by
functionality (e.g. `dt_summarize()` & `dt_mutate()`)

  - A single bare column can be passed with `by = z`
  - Multiple columns can be passed with `by = list(y, z)` or `by = .(y,
    z)`

<!-- end list -->

``` r
example_dt %>%
  dt_summarize(avg_x = mean(x),
               count = .N,
               by = z)
#>    z avg_x count
#> 1: a   1.5     2
#> 2: b   3.0     1
```

## Enhanced selection

Enhanced selection allows you to mix predicates like `is.double` with
normal selection. Some examples:

``` r
example_dt <- data.table(a = c(1,2,3),
                         b = c(4,5,6),
                         c = c("a","a","b"),
                         d = c("a","b","c"))

example_dt %>%
  dt_select(is.numeric, d)
#>    a b d
#> 1: 1 4 a
#> 2: 2 5 b
#> 3: 3 6 c
```

You can also use this format to drop columns:

``` r
example_dt %>%
  dt_select(-is.numeric)
#>    c d
#> 1: a a
#> 2: a b
#> 3: b c
```

Currently supported:
`is.numeric`/`is.integer`/`is.double`/`is.character`/`is.factor`

#### New variant: `dt_mutate_across()`

Enhanced selection allows the user to replace `dt_mutate_if()` &
`dt_mutate_at()` with one helper - `dt_mutate_across()`.

Using `_across()` instead of `_if()`:

``` r
example_dt <- data.table(a = c(1,1,1),
                         b = c(1,1,1),
                         c = c("a","a","b"),
                         d = c("a","b","c"))

example_dt %>%
  dt_mutate_across(is.numeric, as.character)
#>    a b c d
#> 1: 1 1 a a
#> 2: 1 1 a b
#> 3: 1 1 b c
```

Using `_across()` instead of `_at()`:

``` r
example_dt %>%
  dt_mutate_across(c(a, b), ~ .x + 1)
#>    a b c d
#> 1: 2 2 a a
#> 2: 2 2 a b
#> 3: 2 2 b c
```

These two approaches can be combined in one call:

``` r
example_dt <- data.table(dbl_col1 = c(1.0,1.0,1.0),
                         dbl_col2 = c(1.0,1.0,1.0),
                         int_col1 = c(1L,1L,1L),
                         int_col2 = c(1L,1L,1L),
                         char_col1 = c("a","a","a"),
                         char_col2 = c("b","b","b"))

example_dt %>%
  dt_mutate_across(c(is.double, int_col1), ~ .x + 1)
#>    dbl_col1 dbl_col2 int_col1 int_col2 char_col1 char_col2
#> 1:        2        2        2        1         a         b
#> 2:        2        2        2        1         a         b
#> 3:        2        2        2        1         a         b
```

## `rlang` compatibility

`rlang` quoting/unquoting can be used to write custom functions with
`tidytable` functions.

Note that quosures are not compatible with `data.table`, so `enexpr()`
must be used instead of `enquo()`.

##### Custom function with `dt_mutate()`

``` r
library(rlang)

example_dt <- data.table(x = c(1,1,1), y = c(1,1,1), z = c("a","a","b"))

add_one <- function(.data, new_name, add_col) {
  new_name <- enexpr(new_name)
  add_col <- enexpr(add_col)
  
  .data %>%
    dt_mutate(!!new_name := !!add_col + 1)
}

example_dt %>%
  add_one(x_plus_one, x)
#>    x y z x_plus_one
#> 1: 1 1 a          2
#> 2: 1 1 a          2
#> 3: 1 1 b          2
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

## `dt()` helper

The `dt()` function makes regular `data.table` syntax pipeable, so you
can easily mix `tidytable` syntax with `data.table`
syntax:

``` r
example_dt <- data.table(x = c(1,2,3), y = c(4,5,6), z = c("a", "a", "b"))

example_dt %>%
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

### Speed Comparisons

Below are some speed comparisons of various functions. More functions
will get added to the speed comps over time.

A few notes:

  - The main metric of interest is the time comparison of a tidyverse
    function vs. its equivalent tidytable function.
      - Comparing times from separate functions won’t be very useful.
        For example - the `summarize()` tests were performed on a
        different dataset from `case_when()`.
  - `setDTthreads(1)` was used to ensure a fair comparison to the
    `tidyverse`.
  - `copy(dt)` was used when adding columns in `data.table` to ensure a
    fair comparison to `dplyr::mutate()`.
  - `dt_fill()` & `tidyr::fill()` both work with
    character/factor/logical columns, whereas `data.table::nafill()`
    does not. Testing only included numeric columns due to this
    constraint.
  - Currently `data.table` doesn’t have its own `case_when()`
    translation, so a multiple nested `fifelse()` was used.
  - All tests can be found in the source code of the README.
  - Lastly I’d like to mention that these tests were not rigorously
    created to cover all angles equally. They are just meant to be used
    as general insight into the performance of these packages.

<!-- end list -->

``` r
all_marks
#> # A tibble: 12 x 5
#>    function_tested tidyverse tidytable data.table tidytable_vs_tidyverse
#>    <chr>           <chr>     <chr>     <chr>      <chr>                 
#>  1 arrange         322.3ms   49.3ms    41.2ms     15.3%                 
#>  2 case_when       434ms     147ms     129ms      33.9%                 
#>  3 fill            1030ms    596.61ms  548.41ms   57.9%                 
#>  4 filter          229ms     199ms     214ms      86.9%                 
#>  5 inner_join      65.6ms    82.4ms    74.7ms     125.6%                
#>  6 left_join       74.9ms    109ms     107.1ms    145.5%                
#>  7 mutate          55.2ms    45.8ms    45.8ms     83.0%                 
#>  8 nest            7.07ms    3.65ms    2.81ms     51.6%                 
#>  9 pivot_longer    87.2ms    20.7ms    12.9ms     23.7%                 
#> 10 pivot_wider     715ms     271ms     273ms      37.9%                 
#> 11 summarize       488ms     258ms     253ms      52.9%                 
#> 12 unnest          170.9ms   9.76ms    6.62ms     5.7%
```
