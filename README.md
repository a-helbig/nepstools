
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nepstools

<!-- badges: start -->

<!-- badges: end -->

This package facilitates working with R and NEPS SUF data. Its main
feature — and the core of the package — is a function that efficiently
reads NEPS SUF files in statas dta format. It allows you to specify
German or English language, a switch to instrument variable names and
access to all attached meta information on variables. It builds upon
haven’s read_dta() and readstata13’s read.dta13() functions, leveraging
the strengths of both. The package is inspired by the stata ado
[nepstools](https://www.neps-data.de/Datenzentrum/Forschungsdaten/Datentools-f%C3%BCr-Stata)
developed by the FDZ of the Lifbi in Bamberg, Germany.

## Installation

You can install the development version of *nepstools* with remotes
package like so:

``` r
# install.packages("remotes") - uncomment this row in case you havent installed package "remotes" yet
remotes::install_github("a-helbig/nepstools")
```

## Example

We need to load a NEPS dataset that includes variable and value labels
in English, and also want to access the question texts associated with
the variables.

Using haven::read_dta() did not provide access to the English labels or
the question texts. We also tried readstata13::read.dta13(), but
although it loads metadata (including English labels), these were only
attached at the dataset level, not directly to the variables.
Additionally, this function tends to be relatively slow when working
with larger datasets.

To achieve our goal, we can use the read_neps() function as demonstrated
below:

``` r
library(nepstools)

# File path to publicly available NEPS SC6 semantic structural file on gaps in lifecourse that is included in this package
path <- system.file("extdata", "SC6_spGap_S_15-0-0.dta", package = "nepstools")

# read data with english labels and meta
df_neps <- read_neps(path, english = TRUE)

# print head of data
head(df_neps)
#> # A tibble: 1 × 36
#>    ID_t wave      nepswave  splink spell subspell spgen    spext spstat disagint
#>   <dbl> <dbl+lbl> <dbl+lbl>  <dbl> <dbl>    <dbl> <dbl+lb> <dbl> <dbl+> <dbl+lb>
#> 1    NA NA        NA            NA    NA       NA NA       NA    NA     NA      
#> # ℹ 26 more variables: disagwave <dbl+lbl>, ts29101_v1 <dbl+lbl>,
#> #   ts29901 <dbl+lbl>, ts29300 <dbl+lbl>, ts29101 <dbl+lbl>, ts29102_O <chr>,
#> #   ts2911m <dbl+lbl>, ts2911y <dbl+lbl>, ts29103 <dbl+lbl>, ts2912m <dbl+lbl>,
#> #   ts2912y <dbl+lbl>, ts2912c <dbl+lbl>, ts2911m_g1 <dbl+lbl>,
#> #   ts2911y_g1 <dbl+lbl>, ts2912m_g1 <dbl+lbl>, ts2912y_g1 <dbl+lbl>,
#> #   ts2912c_g1 <dbl+lbl>, spms <dbl+lbl>, ts29201 <dbl+lbl>, ts27108 <dbl+lbl>,
#> #   ts27100 <dbl+lbl>, ts2731m <dbl+lbl>, ts2731y <dbl+lbl>, …

# Example Variable: ts2912m - enddate (month) of gap episode
# Print english variable label 
attr(df_neps$ts2912m, "label")
#> [1] "End month gap"
# Print english questiontext with convenient wrapper nepstools::question()
question(df_neps, "ts29103")
#> [1] "Just to make sure: Are you still retired today?"
```

In addition the package provides a handful of smaller functions that
facilitate working with NEPS data:

1.  `replace_values_with_na()`: replacing NEPS missing values with NA
2.  `replace_season_codes()`: recoding season codes to corresponding
    months
3.  `expand()`: expanding episode data by duration
4.  `question()`: convenient wrapper for printing question texts

For more details, please checkout the *nepstools* vignette.
