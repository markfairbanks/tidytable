
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidytable <img src="man/figures/logo.png" align="right" width="16%" height="16%" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/tidytable)](https://cran.r-project.org/package=tidytable)
[![](https://img.shields.io/badge/dev%20-0.5.2.9-green.svg)](https://github.com/markfairbanks/tidytable)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/last-month/tidytable?color=grey)](https://markfairbanks.github.io/tidytable/)
<!-- badges: end -->

#### Why `tidytable`?

  - `tidyverse`-like syntax with `data.table` speed
  - `rlang` compatibility - [See
    here](https://markfairbanks.github.io/tidytable/#rlang-compatibility)
  - Includes functions that
    [`dtplyr`](https://github.com/tidyverse/dtplyr) is missing,
    including many `tidyr` functions

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

### tidytable helpers

  - `dt()`: Pipeable `data.table` syntax. [See
    here](https://markfairbanks.github.io/tidytable/#dt-helper)
  - `get_dummies.()`
  - `%notin%`

### dplyr

##### Core verbs

  - `arrange.()`
  - `filter.()`
  - `mutate.()` & `mutate_across.()`
      - The `_across.()` helper is new and can replace the
        `_if.()`/`_at.()`/`_all.()` helpers [See
        here](https://markfairbanks.github.io/tidytable/#new-variant-dt_mutate_across)
  - `select.()`
  - `summarize.()` & `summarize_across.()`
      - Group by specifications called inside. [See
        here](https://markfairbanks.github.io/tidytable/#using-group-by)

##### Other dplyr functions

  - `bind_cols.()` & `bind_rows.()`
  - `case.()`: Similar to `dplyr::case_when()`. See `?case.` for syntax
  - `count.()`
  - `distinct.()`
  - `ifelse.()`
  - Joins:
      - `left_join.()`, `inner_join.()`, `right_join.()`,
        `full_join.()`, & `anti_join.()`
  - `lags.()` & `leads.()`
  - `pull.()`
  - `relocate.()`
  - `rename.()` & `rename_with.()`
  - `row_number.()`
  - `slice.()`: `_head.()`/`_tail.()`/`_max.()`/`_min.()`
  - `transmute.()`

### tidyr

  - `drop_na.()`
  - `fill.()`: Works on character/factor/logical types
    (`data.table::nafill()` does not)
  - `group_split.()`
  - Nesting: `nest_by.()` & `unnest.()`
  - `pivot_longer.()` & `pivot_wider.()`
  - `replace_na.()`
  - `separate.()`
  - `separate_rows.()`
  - `unite.()`

### purrr

  - `map.()`, `map2.()`, `map_*.()` variants, & `map2_*.()` variants

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
#>        x     y     z double_x double_y
#>    <dbl> <dbl> <chr>    <dbl>    <dbl>
#> 1:     1     4     a        2        8
#> 2:     2     5     a        4       10
#> 3:     3     6     b        6       12
```

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
#>        z avg_x count
#>    <chr> <dbl> <int>
#> 1:     a   1.5     2
#> 2:     b   3.0     1
```

*Note: The `.by` argument was called `by` in versions of `tidytable`
prior to `v0.5.2`.*

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
  select.(where(is.numeric), d)
#>        a     b     d
#>    <dbl> <dbl> <chr>
#> 1:     1     4     a
#> 2:     2     5     b
#> 3:     3     6     c
```

You can also use this format to drop columns:

``` r
test_df %>%
  select.(-where(is.numeric))
#>        c     d
#>    <chr> <chr>
#> 1:     a     a
#> 2:     a     b
#> 3:     b     c
```

These same ideas can be used whenever selecting columns in `tidytable`
functions - for example when using `count.()`, `drop_na.()`,
`pivot_longer.()`, `pivot_wider.()`, etc.

#### New helper: `mutate_across.()`

`tidyselect` allows the user to replace `mutate_if.()`, `mutate_at.()`,
and `mutate_all.()` with one helper - `mutate_across.()`.

Using `_across.()` instead of `_if.()`:

``` r
test_df <- data.table(a = c(1,1,1),
                      b = c(1,1,1),
                      c = c("a","a","b"),
                      d = c("a","b","c"))

test_df %>%
  mutate_across.(where(is.numeric), as.character)
#>        a     b     c     d
#>    <chr> <chr> <chr> <chr>
#> 1:     1     1     a     a
#> 2:     1     1     a     b
#> 3:     1     1     b     c
```

Using `_across.()` instead of `_at.()`:

``` r
test_df %>%
  mutate_across.(c(a, b), ~ .x + 1)
#>        a     b     c     d
#>    <dbl> <dbl> <chr> <chr>
#> 1:     2     2     a     a
#> 2:     2     2     a     b
#> 3:     2     2     b     c
```

Using `_across.()` instead of `_all.()`:

``` r
test_df %>%
  mutate_across.(everything(), as.factor)
#>        a     b     c     d
#>    <fct> <fct> <fct> <fct>
#> 1:     1     1     a     a
#> 2:     1     1     a     b
#> 3:     1     1     b     c
```

## `rlang` compatibility

`rlang` can be used to write custom functions with `tidytable`
functions.

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
    mutate.(new_col = {{add_col}} + 1)
}

df %>%
  add_one(x)
#>        x     y     z new_col
#>    <dbl> <dbl> <chr>   <dbl>
#> 1:     1     1     a       2
#> 2:     1     1     a       2
#> 3:     1     1     b       2
```

##### Custom function with `summarize.()`

``` r
df <- data.table(x = 1:10, y = c(rep("a", 6), rep("b", 4)), z = c(rep("a", 6), rep("b", 4)))

find_mean <- function(data, grouping_cols, col) {
  data %>%
    summarize.(avg = mean({{col}}),
               .by = {{grouping_cols}})
}

df %>%
  find_mean(grouping_cols = c(y, z), col = x)
#>        y     z   avg
#>    <chr> <chr> <dbl>
#> 1:     a     a   3.5
#> 2:     b     b   8.5
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
  dt(, ':='(double_x = x * 2,
            double_y = y * 2)) %>%
  dt(, list(avg_x = mean(x)), by = z)
#>        z avg_x
#>    <chr> <dbl>
#> 1:     a   1.5
#> 2:     b   3.0
```

### Speed Comparisons

Below are some speed comparisons of various functions. More functions
will get added to the speed comps over time.

A few notes:

  - Comparing times from separate functions won’t be very useful. For
    example - the `summarize()` tests were performed on a different
    dataset from `case_when()`.
  - `setDTthreads(4)` was used for `data.table` & `tidytable` timings.
  - Modify-by-reference was used in `data.table` when being compared to
    `mutate.()` & `dplyr::mutate()`
  - `fill.()` & `tidyr::fill()` both work with character/factor/logical
    columns, whereas `data.table::nafill()` does not. Testing only
    included numeric columns due to this constraint.
  - Currently `data.table` doesn’t have its own `case_when()`
    translation, so a multiple nested `fifelse()` was used.
  - All tests can be found in the source code of the README.
  - `pandas` comparisons are in the process of being added - more will
    be added soon.
  - Lastly I’d like to mention that these tests were not rigorously
    created to cover all angles equally. They are just meant to be used
    as general insight into the performance of these packages.

<!-- end list -->

``` r
all_marks
#> # A tibble: 13 x 6
#>    function_tested data.table tidytable tidyverse pandas tidytable_vs_tidyverse
#>    <chr>           <chr>      <chr>     <chr>     <chr>  <chr>                 
#>  1 arrange         62.66ms    71.67ms   2508.33ms 355ms  2.9%                  
#>  2 case_when       53.51ms    58.61ms   349.07ms  59.2ms 16.8%                 
#>  3 distinct        35.35ms    39.72ms   49.91ms   309ms  79.6%                 
#>  4 fill            39.11ms    54.32ms   57.47ms   846ms  94.5%                 
#>  5 filter          223.77ms   229.91ms  258.51ms  707ms  88.9%                 
#>  6 inner_join      64.57ms    60.66ms   95.56ms   <NA>   63.5%                 
#>  7 left_join       73.61ms    140.16ms  156.04ms  <NA>   89.8%                 
#>  8 mutate          50.14ms    49.51ms   197.46ms  86.4ms 25.1%                 
#>  9 nest            13.59ms    20.7ms    32.43ms   <NA>   63.8%                 
#> 10 pivot_longer    18.77ms    12.16ms   55.06ms   <NA>   22.1%                 
#> 11 pivot_wider     142.66ms   137.34ms  76.79ms   <NA>   178.9%                
#> 12 summarize       319.56ms   169.35ms  217.67ms  834ms  77.8%                 
#> 13 unnest          26.44ms    19.52ms   34.92ms   <NA>   55.9%
```
