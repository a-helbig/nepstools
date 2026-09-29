################################################################################
# test: rename_by_label
################################################################################

make_labelled_df <- function() {
  df <- data.frame(id = 1:2, a = 1:2, b = 1:2, c = 1:2, d = 1:2, e = 1:2)
  attr(df$id, "label") <- "Target-ID"
  attr(df$a, "label")  <- "Age mother"
  attr(df$b, "label")  <- "Age father"
  attr(df$c, "label")  <- "Birth place mother"
  attr(df$d, "label")  <- "Birth place father"
  attr(df$e, "label")  <- "Income"
  df
}

test_that("variables get the first label word, more words only on clashes", {
  res <- rename_by_label(make_labelled_df())
  expect_equal(names(res),
               c("target", "age_mother", "age_father",
                 "birth_place_mother", "birth_place_father", "income"))
})

test_that("labels and values are kept", {
  df  <- make_labelled_df()
  res <- rename_by_label(df)
  expect_equal(attr(res$age_mother, "label"), "Age mother")
  expect_equal(unname(as.list(res)), unname(as.list(df)))
})

test_that("identical labels get numeric suffixes", {
  df <- data.frame(x = 1, y = 1, z = 1)
  for (v in names(df)) attr(df[[v]], "label") <- "Same label"
  expect_equal(names(rename_by_label(df)),
               c("same_label_1", "same_label_2", "same_label_3"))
})

test_that("variables without label keep their name as basis", {
  df <- data.frame(foo = 1, bar = 2)
  attr(df$bar, "label") <- "Some thing"
  expect_equal(names(rename_by_label(df)), c("foo", "some"))
})

test_that("umlauts are transliterated and special characters removed", {
  df <- data.frame(x = 1)
  attr(df$x, "label") <- "Art der L\u00fccke (gro\u00df)"
  expect_equal(names(rename_by_label(df, min_words = 4)), "art_der_luecke_gross")
})

test_that("vars restricts renaming and new names avoid kept names", {
  df <- make_labelled_df()
  names(df)[1] <- "age"
  res <- rename_by_label(df, vars = c(a, b))
  expect_equal(names(res), c("age", "age_mother", "age_father", "c", "d", "e"))

  df2 <- data.frame(income = 1, x = 2)
  attr(df2$x, "label") <- "Income"
  expect_equal(names(rename_by_label(df2, vars = x)), c("income", "income_1"))
})

test_that("exclude keeps variables and is equivalent to negative vars", {
  df <- make_labelled_df()
  res1 <- rename_by_label(df, exclude = c(id, e))
  res2 <- rename_by_label(df, vars = -c(id, e))
  expect_equal(names(res1)[c(1, 6)], c("id", "e"))
  expect_equal(names(res1), names(res2))
})

test_that("tidyselect helpers and character vectors work", {
  df <- make_labelled_df()
  expect_equal(names(rename_by_label(df, vars = dplyr::starts_with("a"))),
               c("id", "age", "b", "c", "d", "e"))
  expect_equal(names(rename_by_label(df, vars = c("a", "b"))),
               c("id", "age_mother", "age_father", "c", "d", "e"))
})

test_that("sep, lower and min_words are respected", {
  df  <- make_labelled_df()
  res <- rename_by_label(df, sep = ".", lower = FALSE, min_words = 2)
  expect_equal(names(res),
               c("Target.ID", "Age.mother", "Age.father",
                 "Birth.place.mother", "Birth.place.father", "Income"))
})

test_that("invalid input gives errors", {
  df <- make_labelled_df()
  expect_error(rename_by_label(1:3))
  expect_error(rename_by_label(df, vars = a, exclude = b), "not both")
  expect_error(rename_by_label(df, min_words = 0))
  expect_error(rename_by_label(df, vars = not_there))
})

test_that("works on NEPS data read with read_neps", {
  res <- rename_by_label(semantic_neps_gap)
  expect_equal(ncol(res), ncol(semantic_neps_gap))
  expect_false(anyDuplicated(names(res)) > 0)
})
