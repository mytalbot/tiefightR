
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tiefightR

<!-- badges: start -->

<!-- badges: end -->

The goal of tiefightR is to …

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mytalbot/tiefightR")
```

tiefightR uses the following packages as dependencies (in no particular
order):  
magrittr, tibble, dplyr, reshape2, prefmod, gnm, ggplot2, parallel,
foreach, viridis

## Example

This is a basic example which shows you how to compare an item with any
combination of the remaining items.

``` r
library(tiefightR)
raw        <- tiefightR::human
human_test <- tie_worth(xdata = raw,
                        showplot  = TRUE,
                        compstudy = "LagreValenceRange_SpringSchool",
                        default   = "War",
                        ordn      = c("Cat", "Crow", "Doctor", "Frustrated", "Lake", "War", "Fire"),
                        r1        = "Lake", # change this
                        r2        = "Cat")  # change this / to multiple combis
```

<img src="man/figures/README-example-1.png" width="100%" />
