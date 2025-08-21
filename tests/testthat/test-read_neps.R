datapath <- "C:/r_projects/sc6-prep-personyear/raw-data2/SC6_Basics_D_14-0-0.dta"
datapath2 <- "C:/r_projects/sc6-prep-personyear/raw-data2/SC6_spGap_D_14-0-0.dta"
datapath3 <- "C:/Diss/Daten und Scripte/Daten/tasks_kldb1988_3.dta"

test_that("is the generated object of type dataframe?", {
  expect_true(is.data.frame(read_neps(datapath)))
})

test_that("will the function error when a non-neps dataset is being loaded", {
  expect_error(read_neps(datapath3))
})

test_that("does the col select argument work?", {
  expect_equal(ncol(read_neps(datapath, col_select = "ID_t")), 1)
  expect_equal(ncol(read_neps(datapath, col_select = NULL)), 99)
})

test_that("does the english argument work?", {
  expect_equal(attr(read_neps(datapath, english =T)$ID_t, "label"), "ID target")
  expect_equal(attr(read_neps(datapath, english =F)$ID_t, "label"), "Target-ID")
})

test_that("does the compact_meta argument work?", {
  expect_equal(length(attributes(read_neps(datapath, compact_meta = F)$ID_t)), 8)
  expect_equal(length(attributes(read_neps(datapath, compact_meta = T)$ID_t)), 3)
})

test_that("does the charren argument work?", {
  expect_equal(names(read_neps(datapath2, charren = F)[13]), "ts29901")
  expect_equal(names(read_neps(datapath2, charren = T)[13]), "h_aktlue")
})

test_that("error is thrown if dataset path is invalid", {
  expect_error(read_neps("non_existing_file.dta"), regexp = "Dataset file does not exist at the specified path.")
})

test_that("error if col_select contains variables not present in data", {
  expect_error(read_neps(datapath, col_select = c("nonexistent_var")))
})

test_that("meta attributes are attached to variables", {
  data <- read_neps(datapath, compact_meta = TRUE)

  # Pick a variable known to have attributes, e.g., "ID_t" and extract alias meta
  alias <- attr(data$ID_t, "NEPS_alias")

  # check if the alias equals ID_t
  expect_match(alias, "ID_t")
})

test_that("meta types filtered correctly based on language", {
  # Extract meta_data used inside function for english = FALSE
  meta_de <- unique(read_exp_fields(datapath)$type)
  meta_de2 <- meta_de[!stringr::str_detect(meta_de, "_en$")]

  # Extract meta_data for english = TRUE
  meta_en <- unique(read_exp_fields(datapath)$type)
  meta_en2 <- meta_en[!stringr::str_detect(meta_en, "_de$")]

  # Check that all meta infos in the vector either contain _de or not end with _en for german meta infos and the other way around for english meta infos
  expect_true(all(stringr::str_detect(meta_de2, "_de") | !stringr::str_detect(meta_de2, "_en$")) )
  expect_true(all(stringr::str_detect(meta_en2, "_en") | !stringr::str_detect(meta_en2, "_de$")) )
})

test_that("switch_var_names replaces variable names with NEPS_alias attribute", {
  df <- data.frame(a = 1, b = 2)
  attr(df$a, "NEPS_alias") <- "var_a"
  attr(df$b, "NEPS_alias") <- ""
  # Use helper function on df
  df_renamed <- switch_var_names(df)

  # Now check that var_a is a new var name and that b still exists as a varname because the alias for b was empty. Also "a" as a varname should no longer be existent
  expect_true("var_a" %in% names(df_renamed))
  expect_true("b" %in% names(df_renamed))
  expect_false("a" %in% names(df_renamed))
})

test_that("read_exp_fields returns a dataframe with expected columns", {
  df_exp_fields <- read_exp_fields(datapath)

  expect_true(is.data.frame(df_exp_fields))
  expect_true(all(c("variable", "type", "value") %in% names(df_exp_fields)))
})

