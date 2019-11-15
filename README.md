
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gdt

<!-- badges: start -->

<!-- badges: end -->

The goal of gdt is to make {data.table} easier to use.

## Installation

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mtfairbanks/gdt")
```

## Example

The alias `let()` can be used to one or more columns:

``` r
library(data.table)
library(gdt)
#> Warning: changing locked binding for '[.data.table' in 'data.table' whilst
#> loading 'gdt'
dt <- data.table(x = c(1,2,3), y = c(4,5,6))

dt[, let(double_x = x * 2,
         double_y = y * 2)][]
#>    x y double_x double_y
#> 1: 1 4        2        8
#> 2: 2 5        4       10
#> 3: 3 6        6       12
```

The alias `agg()` can be used for aggregations:

``` r
dt <- data.table(x = c(1,2,3), y = c("a","a","b"))

dt[, agg(avg_x = mean(x)), by = y]
#>    y avg_x
#> 1: a   1.5
#> 2: b   3.0
```
