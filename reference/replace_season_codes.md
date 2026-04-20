# Replace season codes with months

\`replace_season_codes()\` replaces NEPS specific season codes in date
variables to standard month codes. For example code 27: "Summer" will be
replaced with Code 7: "July".

## Usage

``` r
replace_season_codes(
  data,
  vars = NULL,
  values_to_replace = c(21, 24, 27, 30, 32)
)
```

## Arguments

- data:

  A dataset to apply the function to.

- vars:

  Specify vars where season codes should be replaced with months.
  Optional, if set to NULL, all vars are being taken into account. The
  function than only select variables that have the label "month" or
  "monat". It is recommended to specify the variables to which the
  function should be applied.

- values_to_replace:

  These are the season codes. Usually doesnt need to be modified.

## Value

A Dataframe.

## Examples

``` r
# Example with NEPS SC6 semantic structures spGap file
path <- system.file("extdata", "SC6_spGap_S_15-0-0.dta", package = "nepstools")
df_neps <- read_neps(path, english = TRUE, col_select = c("ID_t", "ts2912m"))

# create some artificial datapoints
artificial_datapoints <- data.frame(
 ID_t = c(1, 2, 3, 24, 5),
 ts2912m = c(-97, 24, 5, 32, 30))

# add these artificial datapoints to the empty neps dataset
df_neps <- rbind(df_neps, artificial_datapoints)
print(df_neps)
#> # A tibble: 6 × 2
#>    ID_t ts2912m              
#>   <dbl> <dbl+lbl>            
#> 1    NA  NA                  
#> 2     1 -97 [refused]        
#> 3     2  24 [Spring/Easter]  
#> 4     3   5 [May]            
#> 5    24  32 [End of the year]
#> 6     5  30 [Fall]           

df_neps_replaced <- replace_season_codes(df_neps)
print(df_neps_replaced)
#> # A tibble: 6 × 2
#>    ID_t ts2912m       
#>   <dbl> <dbl+lbl>     
#> 1    NA  NA           
#> 2     1 -97 [refused] 
#> 3     2   4 [April]   
#> 4     3   5 [May]     
#> 5    24  12 [December]
#> 6     5  10 [October] 
```
