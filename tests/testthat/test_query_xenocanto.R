test_that("search Phaethornis check rows", {

  df1 <- query_xenocanto(term = 'Phaethornis anthophilus')

  expect_true(nrow(df1) > 10)

  })

test_that("no recs found", {

  df1 <- query_xenocanto(term = '000')

  expect_true(is.null(df1))

})


test_that("check messages", {

  msg <- capture.output(query_xenocanto(term = '000', verbose = TRUE))

  expect_true(any(grepl("No audios were found", msg)))

  msg <- capture.output(a <- query_xenocanto(term = '000', verbose = FALSE))


  expect_true(length(msg) == 0)


  msg <- capture.output(a <- query_xenocanto(term = 'Phaethornis anthophilus', verbose = TRUE, pb = FALSE))

  expect_true(length(msg) == 0)



  })

test_that("test all_data FALSE", {

  df1 <- query_xenocanto(term = 'Phaethornis anthophilus', all_data = FALSE)

  expected_col_names <- c("key", "species", "date", "country", "location", "latitude", "longitude", "file_url", "repository")
  query_col_names <- colnames(df1)
  expect_true(identical(query_col_names, expected_col_names), info = "Column names do not match the expected names")

})
