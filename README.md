
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidytable <img id="logo" src="man/figures/logo.png" align="right" width="17%" height="17%" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/tidytable)](https://cran.r-project.org/package=tidytable)
![r-universe](https://fastverse.r-universe.dev/badges/tidytable)
[![downloads](http://cranlogs.r-pkg.org/badges/grand-total/tidytable?color=blue)](https://r-pkg.org/pkg/tidytable)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/last-month/tidytable?color=blue)](https://markfairbanks.github.io/tidytable/)
[![R-CMD-check](https://github.com/markfairbanks/tidytable/workflows/R-CMD-check/badge.svg)](https://github.com/markfairbanks/tidytable/actions)
<!-- badges: end -->

`tidytable` is a data frame cleaning library for users who need
[`data.table`
speed](https://markfairbanks.github.io/tidytable/articles/speed_comparisons.html)
but prefer `tidyverse`-like syntax.

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

`tidytable` replicates `tidyverse` syntax but uses `data.table` in the
background. In general you can simply use `library(tidytable)` to
replace your existing `dplyr` and `tidyr` code with the faster
`tidytable` equivalents.

A full list of implemented functions can be found
[here](https://markfairbanks.github.io/tidytable/reference/index.html).

``` r
library(tidytable)

df <- data.table(x = 1:3, y = 4:6, z = c("a", "a", "b"))

df %>%
  select(x, y, z) %>%
  filter(x < 4, y > 1) %>%
  arrange(x, y) %>%
  mutate(double_x = x * 2,
         x_plus_y = x + y)
#> # A tidytable: 3 × 5
#>       x     y z     double_x x_plus_y
#>   <int> <int> <chr>    <dbl>    <int>
#> 1     1     4 a            2        5
#> 2     2     5 a            4        7
#> 3     3     6 b            6        9
```

## Applying functions by group

You can use the normal `tidyverse` `group_by()`/`ungroup()` workflow, or
you can use `.by` syntax to reduce typing. Using `.by` in a function is
shorthand for `df %>% group_by() %>% fn() %>% ungroup()`.

-   A single column can be passed with `.by = z`
-   Multiple columns can be passed with `.by = c(y, z)`

``` r
df <- data.table(x = c("a", "a", "b"), y = c("a", "a", "b"), z = 1:3)

df %>%
  summarize(avg_z = mean(z),
            .by = c(x, y))
#> # A tidytable: 2 × 3
#>   x     y     avg_z
#>   <chr> <chr> <dbl>
#> 1 a     a       1.5
#> 2 b     b       3
```

All functions that can operate by group have a `.by` argument built in.
(`mutate()`, `filter()`, `summarize()`, etc.)

The above syntax is equivalent to:

``` r
df %>%
  group_by(x, y) %>%
  summarize(avg_z = mean(z)) %>%
  ungroup()
#> # A tidytable: 2 × 3
#>   x     y     avg_z
#>   <chr> <chr> <dbl>
#> 1 a     a       1.5
#> 2 b     b       3
```

Both options are available for users, so you can use the syntax that you
prefer.

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
  select(a, starts_with("b"))
#> # A tidytable: 3 × 3
#>       a    b1    b2
#>   <int> <int> <int>
#> 1     1     4     7
#> 2     2     5     8
#> 3     3     6     9
```

A full overview of selection options can be found
[here](https://tidyselect.r-lib.org/reference/language.html).

### Using tidyselect in `.by`

`tidyselect` helpers also work when using `.by`:

``` r
df <- data.table(x = c("a", "a", "b"), y = c("a", "a", "b"), z = 1:3)

df %>%
  summarize(avg_z = mean(z),
            .by = where(is.character))
#> # A tidytable: 2 × 3
#>   x     y     avg_z
#>   <chr> <chr> <dbl>
#> 1 a     a       1.5
#> 2 b     b       3
```

## Tidy evaluation compatibility

Tidy evaluation can be used to write custom functions with `tidytable`
functions. The embracing shortcut `{{ }}` works, or you can use
`enquo()` with `!!` if you prefer:

``` r
df <- data.table(x = c(1, 1, 1), y = 4:6, z = c("a", "a", "b"))

add_one <- function(data, add_col) {
  data %>%
    mutate(new_col = {{ add_col }} + 1)
}

df %>%
  add_one(x)
#> # A tidytable: 3 × 4
#>       x     y z     new_col
#>   <dbl> <int> <chr>   <dbl>
#> 1     1     4 a           2
#> 2     1     5 a           2
#> 3     1     6 b           2
```

The `.data` and `.env` pronouns also work within `tidytable` functions:

``` r
var <- 10

df %>%
  mutate(new_col = .data$x + .env$var)
#> # A tidytable: 3 × 4
#>       x     y z     new_col
#>   <dbl> <int> <chr>   <dbl>
#> 1     1     4 a          11
#> 2     1     5 a          11
#> 3     1     6 b          11
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

## `verb.()` syntax

For backwards compatibility `tidytable` exports `verb.()` versions of
functions. This will also allow users to more easily combine `dplyr` and
`tidytable` functions in one script:

``` r
df <- data.table(x = 1:3, y = 4:6, z = c("a", "a", "b"))

df %>%
  mutate.(double_x = x * 2)
#> # A tidytable: 3 × 4
#>       x     y z     double_x
#>   <int> <int> <chr>    <dbl>
#> 1     1     4 a            2
#> 2     2     5 a            4
#> 3     3     6 b            6
```

## Acknowledgements

`tidytable` is only possible because of the great contributions to R by
the `data.table` and `tidyverse` teams. `data.table` is used as the main
data frame engine in the background, while `tidyverse` packages like
`rlang`, `vctrs`, and `tidyselect` are heavily relied upon to give users
a similar experience to `dplyr` and `tidyr`.
