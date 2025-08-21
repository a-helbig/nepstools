datapath2 <- "C:/r_projects/sc6-prep-personyear/raw-data2/SC6_spGap_D_14-0-0.dta"

################################################################################
# test: replace_value_with_na
################################################################################

test_that("neps missing codes are replaced with NA in all variables", {
  # Call read_neps function to get the dataframe
  df1 <- replace_values_with_na(read_neps(datapath2))
  df2 <- read_neps(datapath2)

  # Define the NEPS missing values vector
  neps_miss_vals <- c(seq(-99, -90), seq(-56, -51), seq(-29, -22))

  # Check each column for NEPS missing values
  for (colname in names(df1)) {
    col_vals <- df1[[colname]]

    # Test that none of the missing values are present in this column
    expect_false(any(col_vals %in% neps_miss_vals),
                 info = paste("NEPS Missing values found in df1 in column", colname))
  }

  # Check if any NEPS missing value exists in any column
  has_miss_vals <- any(sapply(df2, function(col) any(col %in% neps_miss_vals)))

  # Check this for every var in df2
  for (colname in names(df2)) {
    col_vals <- df2[[colname]]

    # Test that none of the forbidden values are present in this column
    expect_true(has_miss_vals, info = "No NEPS missing values found in any variable, allthough they were expected")
  }
  })


test_that("Replaces specified values with NA in all dataframe variables when vars = NULL", {
  df <- data.frame(
    a = c(-99, 1, 2),
    b = c(3, -56, 4)
  )
  result <- replace_values_with_na(df)

  expect_true(all(is.na(result$a[1])))
  expect_true(all(is.na(result$b[2])))

  # Non-matching values remain unchanged
  expect_equal(result$a[2:3], c(1, 2))
  expect_equal(result$b[c(1,3)], c(3, 4))
})

test_that("Replaces specified values with NA only in selected variables", {
  df <- data.frame(
    a = c(-99, 1, 2),
    b = c(3, -56, 4)
  )
  result <- replace_values_with_na(df, vars = "a")

  # a variable has NA replaced
  expect_true(is.na(result$a[1]))

  # b variable remains unchanged
  expect_equal(result$b, c(3, -56, 4))
})

test_that("Replaces values with NA in vector input", {
  vec <- c(-99, 1, -56, 5)
  result <- replace_values_with_na(vec)

  expect_true(is.na(result[1]))
  expect_true(is.na(result[3]))

  # Non-matching values remain unchanged
  expect_equal(result[c(2, 4)], c(1, 5))
})

test_that("Stops with error on unsupported input types", {
  expect_error(replace_values_with_na(matrix(1:4, 2, 2)), "Input data must be either a dataframe or a vector.")
})

test_that("Works if no values_to_replace are found (no replacement occurs)", {
  df <- data.frame(
    a = c(1, 2, 3),
    b = c(4, 5, 6)
  )
  result <- replace_values_with_na(df)

  expect_equal(result, df)

  vec <- c(1, 2, 3)
  result_vec <- replace_values_with_na(vec)
  expect_equal(result_vec, vec)
})

test_that("Works with multiple vars selected", {
  df <- data.frame(
    a = c(-99, 1, -56),
    b = c(-56, 2, -29),
    c = c(-22, 3, 4)
  )

  result <- replace_values_with_na(df, vars = c("a", "c"))

  expect_true(is.na(result$a[1]))
  expect_true(is.na(result$a[3]))

  # b variable is untouched
  expect_equal(result$b, c(-56, 2, -29))

  # Only c[1] replaced with NA
  expect_true(is.na(result$c[1]))
  expect_equal(result$c[2:3], c(3, 4))
})

################################################################################
# test: replace_season_codes
################################################################################
test_that("are all season codes in all month variables, and only in them, replaced with -20?", {
  # Call read_neps function to get the dataframe
  df1 <- replace_season_codes(read_neps(datapath2))

  # Helper function to check if a variable has a label attribute containing "month" or "monat"
  has_month_label <- function(x) {
    lbl <- attr(x, "label")
    if (is.null(lbl)) return(FALSE)
    lbl_lower <- tolower(as.character(lbl))
    return(grepl("month|monat", lbl_lower, ignore.case = TRUE))
  }

  # vector of variable names that have month label
  vars <- names(df1)[sapply(df1, has_month_label)]

  # subset dataset to dataset with only month vars
  df1_months_vars <- df1[vars]

  # test that all vars in that dataset have only values lower/equal to 12
  for (var in vars) {
    expect_true(all(df1[[var]] <= 12 | is.na(df1[[var]])), info = paste("Values in", var, "are > 12"))
  }
})

# test: replace_season_codes
test_that("non-month variables remain unchanged after replacement", {
  df_original <- read_neps(datapath2)
  df_replaced <- replace_season_codes(df_original)

  has_month_label <- function(x) {
    lbl <- attr(x, "label")
    if (is.null(lbl)) return(FALSE)
    lbl_lower <- tolower(as.character(lbl))
    return(grepl("month|monat", lbl_lower, ignore.case = TRUE))
  }

  month_vars <- names(df_original)[sapply(df_original, has_month_label)]
  non_month_vars <- setdiff(names(df_original), month_vars)

  for (var in non_month_vars) {
    expect_equal(
      df_replaced[[var]],
      df_original[[var]],
      info = paste("Non-month variable", var, "has changed after replacement")
    )
  }
})

################################################################################
# expand func
################################################################################
test_that("expand returns a dataframe with correct number of rows", {
  df <- data.frame(id = 1:3, dur = c(2, 1, 3))
  result <- expand(df, dur)

  # number of rows should be sum of durations
  expect_equal(nrow(result), sum(df$dur))

  # columns should match input dataframe columns
  expect_equal(ncol(result), ncol(df))

  # first rows of expanded data correspond to first row in input repeated duration times
  expect_equal(result$id[1:2], rep(df$id[1], 2))
  expect_equal(result$dur[1:2], rep(df$dur[1], 2))
})

test_that("expand errors when duration has NA values", {
  df <- data.frame(id = 1:2, dur = c(1, NA))
  expect_error(expand(df, dur), "Please ensure the duration argument is a valid non-negative number and not NA")
})

test_that("expand errors when duration has negative values", {
  df <- data.frame(id = 1:2, dur = c(1, -1))
  expect_error(expand(df, dur), "Please ensure the duration argument is a valid non-negative number and not NA")
})

test_that("expand works with duration zero (produces zero rows)", {
  df <- data.frame(id = 1:2, dur = c(0, 3))
  result <- expand(df, dur)

  # Rows corresponding to zero duration rows are not replicated (zero times)
  expected_row_count <- sum(df$dur)
  expect_equal(nrow(result), expected_row_count)

  # Specifically, no rows should correspond to the first row
  expect_false(any(result$id == df$id[1]))
})

test_that("expand works with duration equal to 1 (no replication)", {
  df <- data.frame(id = 1:3, dur = rep(1, 3))
  result <- expand(df, dur)

  expect_equal(nrow(result), nrow(df))
  expect_equal(result, df)
})

test_that("expand supports non-standard evaluation for the duration argument", {
  df <- data.frame(id = 1:2, time = c(2, 3))

  # it should not throw error and expand based on 'time' column
  result <- expand(df, time)

  expect_equal(nrow(result), sum(df$time))
})

test_that("expand with a dataframe with one row and duration 1 returns the same row", {
  df <- data.frame(id = 1, dur = 1)
  result <- expand(df, dur)

  expect_equal(nrow(result), 1)
  expect_equal(result, df)
})

################################################################################
# question func
################################################################################

test_that("prints questiontext", {
  df <- data.frame(id = 1, dur = 1)
  attr(df$id, "NEPS_questiontext_de") <- "example question?"

  expect_equal(question(df, "id"), "example question?")
})

test_that("error if variable not in data", {
  df <- data.frame(a = 1:3, b = letters[1:3])
  expect_error(question(df, "c"),
               "Variable 'c' not found in the data frame.")
})

test_that("returns attribute with partial match (non-exact)", {
  # attribute name partially matching "NEPS_questiontext_"
  df <- data.frame(a = 1:3)
  attr(df$a, "NEPS_questiontext_extra") <- "Extra text"
  expect_equal(question(df, "a"), "Extra text")
})

test_that("warns and returns NULL if attribute missing", {
  df <- data.frame(a = 1:3)
  expect_warning(res <- question(df, "a"), "does not have a questiontext attached")
  expect_null(res)
})

test_that("warns and returns NULL if attribute is empty", {
  df <- data.frame(a = 1:3)
  attr(df$a, "NEPS_questiontext_") <- character(0)
  expect_warning(res <- question(df, "a"), "does not have a questiontext attached")
  expect_null(res)
})
