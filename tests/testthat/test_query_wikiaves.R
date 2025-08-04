test_that("search Glaucis dohrnii sound", {

  df1 <- query_wikiaves(term = 'Glaucis dohrnii', type =  "sound")

  expect_true(nrow(df1) >= 30)

})


test_that("search Spatula discors sound (no sounds)", {

  df1 <- query_wikiaves(term = 'Alagoas Tyrannulet', type = "sound")

  expect_true(is.null(df1))

})

test_that("search Glaucis dohrnii photos", {

  df1 <- query_wikiaves(term = 'Glaucis dohrnii', type = "still image")

  expect_true(nrow(df1) >=  420)

})



test_that("no result", {

  df1 <- query_wikiaves(term = 'asdasdasd', type =  "still image")

  expect_true(is.null(df1))

})


test_that("search Glaucis photos in parallel", {

  df1 <- query_wikiaves(term = 'Glaucis dohrnii', type =  "still image", cores = 2)

  expect_true(nrow(df1) >=  77)

})


test_that("test verbose FALSE", {

  df1 <- capture_output(query_wikiaves(term = 'a3', type =  "sound", verbose = FALSE, pb = FALSE))

  expect_true(df1 == "")

})

test_that("test all_data FALSE", {

  df1 <- query_wikiaves(term = 'Glaucis dohrnii', type =  "still image", all_data = FALSE)

  expected_col_names <- c("key", "species", "date", "country", "location", "latitude", "longitude", "file_url", "repository")
  query_col_names <- colnames(df1)
  expect_true(identical(query_col_names, expected_col_names), info = "Column names do not match the expected names")

})
