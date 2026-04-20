# Search for keywords in attributes of variables

This function searches through the attributes of each column (variable)
in a data frame for specified keyword(s) and returns a named list of all
attribute values that contain any of the search words. Data frame-level
attributes are \*\*not searched\*\*. The search is case-insensitive by
default.

## Usage

``` r
lookfor_meta(df, search_words, attr_names = NULL, ignore.case = TRUE)
```

## Arguments

- df:

  A data frame whose columns' attributes will be searched.

- search_words:

  A character vector of one or more keywords to search for within
  attributes.

- attr_names:

  Character vector specifying attribute names to restrict the search to.
  If \`NULL\` (default), all attributes of each variable (column) are
  searched. Only attributes with names matching one of the values in
  \`attr_names\` will be searched when provided.

- ignore.case:

  Logical; if `TRUE` (default), the search is case-insensitive.

## Value

A named list of matched attribute values. The names describe where each
match was found as: `"Variable <column_name>: @<attribute_name>"` If no
matches are found, the function prints a message and returns `NULL`.

## Examples

``` r
# Example with NEPS SC6 semantic structures spGap file
path <- system.file("extdata", "SC6_spGap_S_15-0-0.dta", package = "nepstools")
df_neps <- read_neps(path, english = TRUE)

# Search for keyword "type" to identify all variables dealing with the type of the gap in some way
lookfor_meta(df_neps, "type")
#> $`Variable ts29101_v1: @label`
#> [1] "Type of gap"
#> 
#> $`Variable ts29101: @label`
#> [1] "Type of gap"
#> 
#> $`Variable ts29101: @NEPS_questiontext_en`
#> [1] "[AUTO] Type of gap episode"
#> 
#> $`Variable spms: @label`
#> [1] "Check module: spell type"
#> 
```
