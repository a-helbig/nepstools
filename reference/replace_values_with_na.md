# Set missings in dataframe to NA

\`replace_values_with_na()\` sets NEPS specific missing codes to NA.
Default missing codes are c(seq(-99, -90), seq(-56, -51), seq(-29,
-22)). If only specific variables should be taken into account, use the
vars argument and supply a vector of variable names.

## Usage

``` r
replace_values_with_na(
  data,
  vars = NULL,
  values_to_replace = c(seq(-99, -90), seq(-56, -51), seq(-29, -22))
)
```

## Arguments

- data:

  A dataframe to apply the function to.

- vars:

  Specify variables where missing values should be replaced with NA. If
  set to NULL, all variables will be used.

- values_to_replace:

  Specify values that should be replaced with NA. Default are all
  standard NEPS missing codes.

## Value

A dataframe.

## Examples

``` r
# Example with NEPS SC6 semantic structures spGap file
path <- system.file("extdata", "SC6_spGap_S_15-0-0.dta", package = "nepstools")
df_neps <- read_neps(path, english = TRUE, col_select = c("ID_t", "ts2912m"))

# create some artificial datapoints
artificial_datapoints <- data.frame(
 ID_t = c(1, 2, 3, 4, 5),
 ts2912m = c(-97, 12, NA, 4, -98))

# add these artificial datapoints to the empty neps dataset
df_neps <- rbind(df_neps, artificial_datapoints)
print(df_neps)
#> # A tibble: 6 × 2
#>    ID_t ts2912m         
#>   <dbl> <dbl+lbl>       
#> 1    NA  NA             
#> 2     1 -97 [refused]   
#> 3     2  12 [December]  
#> 4     3  NA             
#> 5     4   4 [April]     
#> 6     5 -98 [don't know]

df_neps_replaced <- replace_values_with_na(df_neps)
print(df_neps_replaced)
#> # A tibble: 6 × 2
#>    ID_t ts2912m      
#>   <dbl> <dbl+lbl>    
#> 1    NA NA           
#> 2     1 NA           
#> 3     2 12 [December]
#> 4     3 NA           
#> 5     4  4 [April]   
#> 6     5 NA           

# Example with a vector
v <- c(1, -97, 3, -29, 5)
replace_values_with_na(v)
#> [1]  1 NA  3 NA  5

```
