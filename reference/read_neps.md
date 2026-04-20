# Read NEPS SUF files in .dta format

\`read_neps()\` reads NEPS SUF data in .dta file format and attracts
available meta infos to each variable. Select variables with col_select,
specify language: German (default) or English and lastly specify if only
most important meta info (default) should be attracted or all of it.

## Usage

``` r
read_neps(
  datasetpath,
  col_select = NULL,
  row_select = Inf,
  english = FALSE,
  compact_meta = TRUE,
  charren = FALSE
)
```

## Arguments

- datasetpath:

  A datapath to a NEPS data file

- col_select:

  Specify variables that will be included in the dataset. If set to
  NULL, data is being loaded with all available variables.

- row_select:

  Specify maximum number of rows to read.

- english:

  If set to TRUE, the dataset will be loaded with English variable and
  value labels, and metadata. If set to FALSE, German labels and
  metadata will be used instead.

- compact_meta:

  If set to TRUE, only a selection of important metadata will be added
  to the data, including question texts, interviewer texts,
  harmonization rules, and instrument variable names, if available. If
  set to FALSE, all available metadata will be included.

- charren:

  If set to TRUE, instrument variable names will replace the standard
  variable names. If set to FALSE, the standard variable names will be
  retained.

## Value

A Dataframe.

## Examples

``` r
path <- system.file("extdata", "SC6_spGap_S_15-0-0.dta", package = "nepstools")
df <- read_neps(path, english = TRUE, charren = TRUE)
print(names(df))
#>  [1] "ID_t"      "wave"      "nepswave"  "splink"    "spell"     "subspell" 
#>  [7] "spgen"     "spext"     "spstat"    "disagint"  "disagwave" "lutyp_v1" 
#> [13] "h_aktlue"  "lumod"     "lutyp"     "lutyps"    "lustm"     "lustj"    
#> [19] "luendre"   "luendm"    "luendj"    "luiz"      "lustm_g1"  "lustj_g1" 
#> [25] "luendm_g1" "luendj_g1" "luiz_g1"   "spms"      "lufb"      "kieuaf12" 
#> [31] "kieubeeg"  "kieuetstm" "kieuetstj" "kieuaf2"   "kieuaf4"   "kieuaf6"  
attr(df$luendm, "NEPS_questiontext_")
#> [1] "(Until when were you <h_modak>?)"
```
