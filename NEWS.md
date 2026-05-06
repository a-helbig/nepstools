# nepstools 0.1.4

- Changed `replace_values_with_na()` so that it now warns users when they specify variables that do not exist in the provided dataset, and ignores those variables.

# nepstools 0.1.3

- Added a citation suggestion to the README and a CITATION file.

# nepstools 0.1.2

## Bug fixes
- Fixed an error in `read_neps()` that occurred when compact metadata was unavailable in a dataset.
- Improved handling of datasets without compact meta fields.
- Added safer checks when filtering metadata and attaching attributes to variables.
