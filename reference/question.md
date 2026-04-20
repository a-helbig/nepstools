# Print questiontext

\`question()\` is a convenient wrapper for the attr() function. It
prints the attached questiontext from specified variable in the variable
argument to the console.

## Usage

``` r
question(data, variable)
```

## Arguments

- data:

  Specify the dataset that was generated with read_neps function

- variable:

  Specify the variable for which the question text should be printed.
  Argument can be a string or an unquoted variable name.

## Value

A String.

## Examples

``` r
# Example with NEPS SC6 semantic structures spGap file
path <- system.file("extdata", "SC6_spGap_S_15-0-0.dta", package = "nepstools")
df_neps <- read_neps(path, english = TRUE, col_select = c("ID_t", "ts2912m"))

question(df_neps, "ts2912m")
#> [1] "(Until when were you <h_modak>?)"
```
