
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ModelMediation

<!-- badges: start -->
<!-- badges: end -->

The goal of ModelMediation is to facilitate estimation and uncertainty
quantification for mediation effects in model based mediation analysis.

Here is an outline of what is left to do:

- Build validation dataset
  - Generate data in a way that can be recycled for parametric bootstrap
- Run bootstrap
  - Parametric bootstrap
  - Non-parametric bootstrap
- Construct CIs from bootstrap output
  - Decide on standard output for previous step
  - Percentile
  - Basic
  - ~~Wald (parametric only)~~ (see note below)
  - Bias Corrected?

For now, I’m going to omit the Wald interval. I think it will be better
to keep parametric and non-parametric as similar as possible and
introduce any differences later.

Important Note: The issues I’ve had with incompatible country order
should be fixed if I sort the data by country before doing any analysis.

## Installation

You can install the development version of ModelMediation from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("wruth1/ModelMediation")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ModelMediation)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
