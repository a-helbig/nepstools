# nepstools

## Introduction

The nepstools package provides convenient functions to work with NEPS
Scientific Use Files (SUFs). It is inspired by the stata ado nepstools
developed by the Forschungsdatenzentrum at LifBi in Bamberg.

This vignette introduces key functionalities for:

- Reading NEPS data files with
  [`read_neps()`](https://a-helbig.github.io/nepstools/reference/read_neps.md)
- Replacing NEPS-specific missing codes with NA with
  [`replace_values_with_na()`](https://a-helbig.github.io/nepstools/reference/replace_values_with_na.md)
- Recoding season codes to corresponding calendar months with
  [`replace_season_codes()`](https://a-helbig.github.io/nepstools/reference/replace_season_codes.md)
- Expanding episode data by episode duration with
  [`expand()`](https://a-helbig.github.io/nepstools/reference/expand.md)
- Extracting variable question texts with
  [`question()`](https://a-helbig.github.io/nepstools/reference/question.md)
- Search for keywords in the attributes of the variables with
  [`lookfor_meta()`](https://a-helbig.github.io/nepstools/reference/lookfor_meta.md)

Note that there is a NEPS semantic data file included in this package
that only contains meta information and one row of NA values (these
files are publicly available on the neps-data website and are therefore
allowed to be in this package). This vignette occasionally includes
artificial data added to the file to illustrate the package’s features.

## 1. Reading NEPS data

The core of this package is the
[`read_neps()`](https://a-helbig.github.io/nepstools/reference/read_neps.md)
function. It reads data from a NEPS Scientific Use File in
**dta-format** and attaches associated metadata such as labels, question
texts, and harmonization rules as attributes of the variables in the
dataframe. Moreover, it allows a language switch of labels and meta
information from German to English.

Under the hood, the function relies on `read_dta()` from the tidyverse
package *haven*, which is very performant with larger datasets compared
to other dta-readers. It does however not support reading (all) attached
meta information in the NEPS-SUF-Files. For this task the function
`read.dta13()` from the package *readstata13* is utilized. Since it does
not have to read the rows of the dataset to get access to the meta
information, performance with large datasets isnt an issue.

With the arguments *col_select* and *row_select* you can specify the
columns and rows that the function will read from the dataset. By
default it will read all columns and rows.

``` r

library(nepstools)

# file path to NEPS semantic file on gaps in lifecourse that is included in this package
path <- system.file("extdata", "SC6_spGap_S_15-0-0.dta", package = "nepstools")

# read data with all cols and rows
df_neps <- read_neps(path)
# print number of cols and rows
print(ncol(df_neps))
#> [1] 36
print(nrow(df_neps))
#> [1] 1
# print head of df
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

# read data with variables ID_t and wave only and with 0 rows 
df_neps <- read_neps(path, col_select = c("ID_t", "wave"), row_select = 0)
# print number of cols and rows
print(ncol(df_neps))
#> [1] 2
print(nrow(df_neps))
#> [1] 0
# print head of df
head(df_neps)
#> # A tibble: 0 × 2
#> # ℹ 2 variables: ID_t <dbl>, wave <dbl+lbl>
```

When argument *english* is set to TRUE, all variable and value labels as
well as attached meta information will be switched from German (default)
to English:

``` r

library(nepstools)

# file path to NEPS semantic file on gaps in lifecourse that is included in this package
path <- system.file("extdata", "SC6_spGap_S_15-0-0.dta", package = "nepstools")

# read data with english labels and meta
df_neps <- read_neps(path, english = TRUE)
# access variable label, value label and questiontext attribute of a variable
attr(df_neps$ts2912m, "label")
#> [1] "End month gap"
attr(df_neps$ts2912m, "labels")
#>                   don't know                      refused 
#>                          -98                          -97 
#>                  until today                      January 
#>                           -5                            1 
#>                     February                        March 
#>                            2                            3 
#>                        April                          May 
#>                            4                            5 
#>                         June                         July 
#>                            6                            7 
#>                       August                    September 
#>                            8                            9 
#>                      October                     November 
#>                           10                           11 
#>                     December Beginning of the year/Winter 
#>                           12                           21 
#>                Spring/Easter              Mid-year/Summer 
#>                           24                           27 
#>                         Fall              End of the year 
#>                           30                           32
attr(df_neps$ts2912m, "NEPS_questiontext")
#> [1] "(Until when were you <h_modak>?)"

# read data with german labels and meta
df_neps <- read_neps(path, english = FALSE)
# access variable label, value label and questiontext attribute of a variable
attr(df_neps$ts2912m, "label")
#> [1] "Endmonat Lücke"
attr(df_neps$ts2912m, "labels")
#>          weiß nicht   Angabe verweigert           bis heute              Januar 
#>                 -98                 -97                  -5                   1 
#>             Februar                März               April                 Mai 
#>                   2                   3                   4                   5 
#>                Juni                Juli              August           September 
#>                   6                   7                   8                   9 
#>             Oktober            November            Dezember Jahresanfang/Winter 
#>                  10                  11                  12                  21 
#>     Frühjahr/Ostern  Jahresmitte/Sommer              Herbst          Jahresende 
#>                  24                  27                  30                  32
attr(df_neps$ts2912m, "NEPS_questiontext")
#> [1] "(Bis wann waren Sie <h_modak>?)"
```

When argument *compact_meta* is set to TRUE (default), only the
following meta infos will be attached if available: questiontext,
interview-instruction-text, harmonization rule and alias. If set to
FALSE, all available meta infos will be attached.

``` r

# read data with minimum available meta info
df_neps <- read_neps(path, compact_meta = TRUE)
# show how many attributes are being attached to the data
length(attributes(df_neps$ts2912m))
#> [1] 8

# read data with all available meta info
df_neps <- read_neps(path, compact_meta = FALSE)
# show how many attributes are being attached to the data
length(attributes(df_neps$ts2912m))
#> [1] 19
```

When the argument *charren* is set to TRUE, variable names are replaced
with instrument names where available. These instrument names may be
more intuitive for some NEPS data users, as they are often more
self-explanatory.

``` r

# read data with minimum available meta info
df_neps <- read_neps(path, charren = TRUE)
# print names of dataset
print(df_neps)
#> # A tibble: 1 × 36
#>    ID_t wave      nepswave  splink spell subspell spgen    spext spstat disagint
#>   <dbl> <dbl+lbl> <dbl+lbl>  <dbl> <dbl>    <dbl> <dbl+lb> <dbl> <dbl+> <dbl+lb>
#> 1    NA NA        NA            NA    NA       NA NA       NA    NA     NA      
#> # ℹ 26 more variables: disagwave <dbl+lbl>, lutyp_v1 <dbl+lbl>,
#> #   h_aktlue <dbl+lbl>, lumod <dbl+lbl>, lutyp <dbl+lbl>, lutyps <chr>,
#> #   lustm <dbl+lbl>, lustj <dbl+lbl>, luendre <dbl+lbl>, luendm <dbl+lbl>,
#> #   luendj <dbl+lbl>, luiz <dbl+lbl>, lustm_g1 <dbl+lbl>, lustj_g1 <dbl+lbl>,
#> #   luendm_g1 <dbl+lbl>, luendj_g1 <dbl+lbl>, luiz_g1 <dbl+lbl>,
#> #   spms <dbl+lbl>, lufb <dbl+lbl>, kieuaf12 <dbl+lbl>, kieubeeg <dbl+lbl>,
#> #   kieuetstm <dbl+lbl>, kieuetstj <dbl+lbl>, kieuaf2 <dbl+lbl>, …

# read data with all available meta infos
df_neps <- read_neps(path, charren = FALSE)
# print names of dataset
print(df_neps)
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
```

## 2. Replacing NEPS missing values with NA

NEPS datasets use various negative codes to indicate different types of
missing values. The
[`replace_values_with_na()`](https://a-helbig.github.io/nepstools/reference/replace_values_with_na.md)
function replaces these codes by NA for easier handling in R.

``` r

# read data with variables: "ID" and "enddate of episode"
df_neps <- read_neps(path, col_select = c("ID_t", "ts2912m"), row_select = 0) 

# create some artificial datapoints 
artificial_datapoints <- data.frame(
  ID_t = c(1, 2, 3, 4, 5),
  ts2912m = c(-97, 12, NA, 4, -98)
)

# add these artificial datapoints to the empty neps dataset
df_neps <- dplyr::bind_rows(df_neps, artificial_datapoints)
print(df_neps)
#> # A tibble: 5 × 2
#>    ID_t ts2912m                
#>   <dbl> <dbl+lbl>              
#> 1     1 -97 [Angabe verweigert]
#> 2     2  12 [Dezember]         
#> 3     3  NA                    
#> 4     4   4 [April]            
#> 5     5 -98 [weiß nicht]

# now use the replace_values_with_na function
df_clean <- replace_values_with_na(df_neps)
print(df_clean)
#> # A tibble: 5 × 2
#>    ID_t ts2912m      
#>   <dbl> <dbl+lbl>    
#> 1     1 NA           
#> 2     2 12 [Dezember]
#> 3     3 NA           
#> 4     4  4 [April]   
#> 5     5 NA
```

If you do want all variables to be considered in the function, you can
specify variables in the *vars* argument. In addition you can provide
custom missing value codes to be considered with the argument
*values_to_replace*. Note, that the function also works on vectors.

## 3. Recoding season codes to corresponding months

In the NEPS study, respondents can answer questions on the date of
events with seasons instead of exact months. The codes for these seasons
are 21,24,27,30 and 32 and correspond to January (1 + 20), April (4 +
20) and so on. Use the function
[`replace_season_codes()`](https://a-helbig.github.io/nepstools/reference/replace_season_codes.md)
to revert these season codes to standard month codes.

``` r

# read data with variables: "ID" and "enddate of episode"
df_neps <- read_neps(path, col_select = c("ID_t", "ts2912m"), row_select = 0)
attr(df_neps$ts2912m, "label")
#> [1] "Endmonat Lücke"

# create some artificial datapoints 
artificial_datapoints <- data.frame(
  ID_t = c(1, 2, 3, 4, 5),
  ts2912m = c(24, 12, 32, 4, -98)
)

# add these artificial datapoints to the empty neps dataset
df_neps <- dplyr::bind_rows(df_neps, artificial_datapoints)
print(df_neps)
#> # A tibble: 5 × 2
#>    ID_t ts2912m              
#>   <dbl> <dbl+lbl>            
#> 1     1  24 [Frühjahr/Ostern]
#> 2     2  12 [Dezember]       
#> 3     3  32 [Jahresende]     
#> 4     4   4 [April]          
#> 5     5 -98 [weiß nicht]
attr(df_neps$ts2912m, "label")
#> [1] "Endmonat Lücke"

# use the function to replace season codes with corresponding months
df_month_recoded <- replace_season_codes(df_neps)
print(df_month_recoded)
#> # A tibble: 5 × 2
#>    ID_t ts2912m         
#>   <dbl> <dbl+lbl>       
#> 1     1   4 [April]     
#> 2     2  12 [Dezember]  
#> 3     3  12 [Dezember]  
#> 4     4   4 [April]     
#> 5     5 -98 [weiß nicht]
```

Note that if the *vars* argument is NULL (the default), only variables
with label attributes containing “month” or “monat” are processed.
Alternatively, you can explicitly specify which variables to include.

## 4. Expanding episode data by duration

Often, episodes or spells are recorded with durations in months or
years. The
[`expand()`](https://a-helbig.github.io/nepstools/reference/expand.md)
function replicates rows according to the length of a specified duration
variable. This function is inspired by statas expand function.

``` r

# read data with variables: "ID" and "enddate of episode"
df_neps <- read_neps(path, col_select = c("ID_t", "ts2911m", "ts2911y", "ts2912m", "ts2912y"), row_select = 0)

# create some artificial datapoints 
artificial_datapoints <- data.frame(
  ID_t = c(1, 1, 2, 2, 3),
  sptype = c(24,26,25,26,27),
  ts2911m = c(2, 5, 11, 3, 12),
  ts2911y = c(2008, 2008, 2005, 2006, 2010),
  ts2912m = c(4, 8, 2, 5, 1),
  ts2912y = c(2008, 2008, 2006, 2006, 2011)
)

# add these artificial datapoints to the empty neps dataset
df_neps <- dplyr::bind_rows(df_neps, artificial_datapoints)

# generate date and duration variables
df_neps <- df_neps |> 
  dplyr::mutate(start = ((ts2911y-1960)*12)+ts2911m - 1, # months since january 1960
                end = ((ts2912y-1960)*12)+ts2912m - 1, # months since january 1960
                duration = (end - start) + 1) # + 1 required so we dont get episode durations of 0 months

print(df_neps)
#> # A tibble: 5 × 9
#>    ID_t ts2911m       ts2911y   ts2912m     ts2912y  sptype start   end duration
#>   <dbl> <dbl+lbl>     <dbl+lbl> <dbl+lbl>   <dbl+lb>  <dbl> <dbl> <dbl>    <dbl>
#> 1     1  2 [Februar]  2008      4 [April]   2008         24   577   579        3
#> 2     1  5 [Mai]      2008      8 [August]  2008         26   580   583        4
#> 3     2 11 [November] 2005      2 [Februar] 2006         25   550   553        4
#> 4     2  3 [März]     2006      5 [Mai]     2006         26   554   556        3
#> 5     3 12 [Dezember] 2010      1 [Januar]  2011         27   611   612        2

# expand the dataframe
expanded_df <- nepstools::expand(df_neps, duration)
print(expanded_df)
#> # A tibble: 16 × 9
#>     ID_t ts2911m       ts2911y   ts2912m     ts2912y sptype start   end duration
#>    <dbl> <dbl+lbl>     <dbl+lbl> <dbl+lbl>   <dbl+l>  <dbl> <dbl> <dbl>    <dbl>
#>  1     1  2 [Februar]  2008      4 [April]   2008        24   577   579        3
#>  2     1  2 [Februar]  2008      4 [April]   2008        24   577   579        3
#>  3     1  2 [Februar]  2008      4 [April]   2008        24   577   579        3
#>  4     1  5 [Mai]      2008      8 [August]  2008        26   580   583        4
#>  5     1  5 [Mai]      2008      8 [August]  2008        26   580   583        4
#>  6     1  5 [Mai]      2008      8 [August]  2008        26   580   583        4
#>  7     1  5 [Mai]      2008      8 [August]  2008        26   580   583        4
#>  8     2 11 [November] 2005      2 [Februar] 2006        25   550   553        4
#>  9     2 11 [November] 2005      2 [Februar] 2006        25   550   553        4
#> 10     2 11 [November] 2005      2 [Februar] 2006        25   550   553        4
#> 11     2 11 [November] 2005      2 [Februar] 2006        25   550   553        4
#> 12     2  3 [März]     2006      5 [Mai]     2006        26   554   556        3
#> 13     2  3 [März]     2006      5 [Mai]     2006        26   554   556        3
#> 14     2  3 [März]     2006      5 [Mai]     2006        26   554   556        3
#> 15     3 12 [Dezember] 2010      1 [Januar]  2011        27   611   612        2
#> 16     3 12 [Dezember] 2010      1 [Januar]  2011        27   611   612        2

# create month indicator - May use a package like lubridate to get a nicer formating for date variables.
expanded_df <- expanded_df |> 
  dplyr::group_by(ID_t) |> 
  dplyr::mutate(month = dplyr::first(start) + dplyr::row_number() - 1)
print(expanded_df)
#> # A tibble: 16 × 10
#> # Groups:   ID_t [3]
#>     ID_t ts2911m       ts2911y ts2912m ts2912y sptype start   end duration month
#>    <dbl> <dbl+lbl>     <dbl+l> <dbl+l> <dbl+l>  <dbl> <dbl> <dbl>    <dbl> <dbl>
#>  1     1  2 [Februar]  2008    4 [Apr… 2008        24   577   579        3   577
#>  2     1  2 [Februar]  2008    4 [Apr… 2008        24   577   579        3   578
#>  3     1  2 [Februar]  2008    4 [Apr… 2008        24   577   579        3   579
#>  4     1  5 [Mai]      2008    8 [Aug… 2008        26   580   583        4   580
#>  5     1  5 [Mai]      2008    8 [Aug… 2008        26   580   583        4   581
#>  6     1  5 [Mai]      2008    8 [Aug… 2008        26   580   583        4   582
#>  7     1  5 [Mai]      2008    8 [Aug… 2008        26   580   583        4   583
#>  8     2 11 [November] 2005    2 [Feb… 2006        25   550   553        4   550
#>  9     2 11 [November] 2005    2 [Feb… 2006        25   550   553        4   551
#> 10     2 11 [November] 2005    2 [Feb… 2006        25   550   553        4   552
#> 11     2 11 [November] 2005    2 [Feb… 2006        25   550   553        4   553
#> 12     2  3 [März]     2006    5 [Mai] 2006        26   554   556        3   554
#> 13     2  3 [März]     2006    5 [Mai] 2006        26   554   556        3   555
#> 14     2  3 [März]     2006    5 [Mai] 2006        26   554   556        3   556
#> 15     3 12 [Dezember] 2010    1 [Jan… 2011        27   611   612        2   611
#> 16     3 12 [Dezember] 2010    1 [Jan… 2011        27   611   612        2   612
```

## 5. Extracting question texts

NEPS variables come with attached metadata, including question texts in
German or English. Use the convenient wrapper
[`question()`](https://a-helbig.github.io/nepstools/reference/question.md)
to print the question text for a specified variable. The language will
depend on the argument english (TRUE or FALSE) in the
[`read_neps()`](https://a-helbig.github.io/nepstools/reference/read_neps.md)
function.

``` r

# read data with variables: "ID" and "enddate of episode"
df_neps <- read_neps(path, col_select = c("ID_t", "ts2912m"))

question(df_neps, "ts2912m")
#> [1] "(Bis wann waren Sie <h_modak>?)"
```

## 6. Search for keywords in attributes of dataframe columns (meta info)

The function
[`lookfor_meta()`](https://a-helbig.github.io/nepstools/reference/lookfor_meta.md)
searches in all attached attributes of each variable in a dataframe
loaded with
[`read_neps()`](https://a-helbig.github.io/nepstools/reference/read_neps.md)
for specified keywords. It is case insensitive by default.

``` r

# read data with variables: "ID" and "enddate of episode"
df_neps <- read_neps(path, english = TRUE)

# Search for keyword "type" to identify all variables dealing with the type of the gap episode in one way or another
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

# Search for keyword "retirement" to identify all variables dealing with the type of the gap episode in one way or another
lookfor_meta(df_neps, "retirement")
#> $`Variable ts29103: @label`
#> [1] "Ongoing of retirement episode"
```
