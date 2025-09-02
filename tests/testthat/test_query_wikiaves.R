test_that("search Glaucis dohrnii sound", {

  skip_on_cran()
  skip_if_offline()

  df1 <- query_wikiaves(term = 'Glaucis dohrnii', format =  "sound")

  expect_true(nrow(df1) >= 30)

})


test_that("search Spatula discors sound (no sounds)", {

  skip_on_cran()
  skip_if_offline()

  df1 <- query_wikiaves(term = 'Alagoas Tyrannulet', format = "sound")

  expect_true(is.null(df1))

})

test_that("search Glaucis dohrnii photos", {

  skip_on_cran()
  skip_if_offline()

  df1 <- query_wikiaves(term = 'Glaucis dohrnii', format = "image")

  expect_true(nrow(df1) >=  420)

})



test_that("no result", {

  skip_on_cran()
  skip_if_offline()

  df1 <- query_wikiaves(term = 'asdasdasd', format =  "image")

  expect_true(is.null(df1))

})


# test_that("search Glaucis photos in parallel", {
#
#   skip_on_cran()
#   skip_if_offline()
#
#   df1 <- query_wikiaves(term = 'Glaucis dohrnii', format =  "image", cores = 2)
#
#   expect_true(nrow(df1) >  400)
#
# })


test_that("test verbose FALSE", {

  skip_on_cran()
  skip_if_offline()

  df1 <- capture_output(query_wikiaves(term = 'a3', format =  "sound", verbose = FALSE, pb = FALSE))

  expect_true(df1 == "")

})

test_that("test all_data FALSE", {

  skip_on_cran()
  skip_if_offline()

  df1 <- query_wikiaves(term = 'Glaucis dohrnii', format =  "image", all_data = FALSE)

  expected_col_names <- .format_query_output(only_basic_columns = T)

  query_col_names <- colnames(df1)

  expect_true(all(query_col_names %in% expected_col_names) & all(expected_col_names %in% query_col_names), info = "Column names do not match the expected names")

})
