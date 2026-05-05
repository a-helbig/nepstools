# Expand data

\`expand()\` duplicates rows by a integer variable specified in the
duration argument, typically a months counter. It is inspired by statas
expand function and often used in the context of episode data that must
be transformed into monthly data.

## Usage

``` r
expand(data, duration)
```

## Arguments

- data:

  A dataframe.

- duration:

  Specify the integer variable to be used for expanding the data. You
  can provide this variable either as a quoted string or unquoted
  variable name. Typically, this is a duration (e.g., episode length)
  variable. It must be a numeric vector containing only non-negative
  integers, with no missing (NA) values. Note: If there are zeros in the
  duration variable, the function will delete these rows. So make sure,
  that any generated duration variables are \> 1 without NAs.

## Value

A Dataframe.

## Examples

``` r

path <- system.file("extdata", "SC6_spGap_S_15-0-0.dta", package = "nepstools")
df_neps_ex <- read_neps(path, col_select = c("ID_t", "ts2911m", "ts2911y", "ts2912m", "ts2912y"))
df_neps_ex <- df_neps_ex[-1,] # get rid of NA row that is in every semantic structure file from NEPS

# create some artificial datapoints
artificial_datapoints <- data.frame(
  ID_t = c(1, 2, 3, 4, 5),
 ts2911m = c(2, 12, 11, 4, 12),
  ts2911y = c(2008, 2006, 2005, 2009, 2010),
  ts2912m = c(4, 1, 2, 5, 2),
  ts2912y = c(2008, 2007, 2006, 2009, 2011)
)

# add these artificial datapoints to the empty neps dataset
df_neps_ex <- rbind(df_neps_ex, artificial_datapoints)

# generate date and duration variables
df_neps_ex <- df_neps_ex |>
  dplyr::mutate(start = ((ts2911y-1960)*12)+ts2911m - 1, # months since january 1960
  end = ((ts2912y-1960)*12)+ts2912m - 1, # months since january 1960
  duration = (end - start) + 1) # + 1 required so we dont get episode durations of 0 months

print(df_neps_ex)
#>   ID_t ts2911m ts2911y ts2912m ts2912y start end duration
#> 1    1       2    2008       4    2008   577 579        3
#> 2    2      12    2006       1    2007   563 564        2
#> 3    3      11    2005       2    2006   550 553        4
#> 4    4       4    2009       5    2009   591 592        2
#> 5    5      12    2010       2    2011   611 613        3

# expand the dataframe
expanded_df <- nepstools::expand(df_neps_ex, duration)
print(expanded_df)
#>     ID_t ts2911m ts2911y ts2912m ts2912y start end duration
#> 1      1       2    2008       4    2008   577 579        3
#> 1.1    1       2    2008       4    2008   577 579        3
#> 1.2    1       2    2008       4    2008   577 579        3
#> 2      2      12    2006       1    2007   563 564        2
#> 2.1    2      12    2006       1    2007   563 564        2
#> 3      3      11    2005       2    2006   550 553        4
#> 3.1    3      11    2005       2    2006   550 553        4
#> 3.2    3      11    2005       2    2006   550 553        4
#> 3.3    3      11    2005       2    2006   550 553        4
#> 4      4       4    2009       5    2009   591 592        2
#> 4.1    4       4    2009       5    2009   591 592        2
#> 5      5      12    2010       2    2011   611 613        3
#> 5.1    5      12    2010       2    2011   611 613        3
#> 5.2    5      12    2010       2    2011   611 613        3
```
