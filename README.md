
# DMC21cent

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of DMC21cent is to suggest possible solutions for Data Monitoring Committee's decision-making processes. 


## Example

This is a basic example which shows you how to get a summary statistic of a subject level data. `sl_summary()` support three kind of plots: `bar_binom` for binary variable, `bar_stack` for binary/categorical variable, and `box` for continuous variables.


``` r
library(DMC21cent)
sl_summary(adsl,
           vars = "SEX",
           kinds = "bar_binom",
           trt = "TRT01P")
```

You can also combine plots as follows:

``` r
library(DMC21cent)
sl_summary(adsl,
           vars = c("SEX", list("SEX" = "M"), "AGE"),
           kinds = c("bar_binom", "bar_binom", "box"),
           titles = c("Female Subjects (%)", "Subjects (%)" ,"AGE Groups"),
           trt = "TRT01A",
           show_plots = TRUE)
```

