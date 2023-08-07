test_that("search Phaethornis check rows", {

  df1 <- query_xenocanto(term = 'Phaethornis anthophilus')

  expect_true(nrow(df1) > 10)

  })


test_that("search Phaethornis check cols", {

  df1 <- query_xenocanto(term = 'Phaethornis anthophilus', all_data = FALSE)

  expect_true(ncol(df1) ==  9)

})


test_that("no recs found", {

  df1 <- query_xenocanto(term = '000')

  expect_true(is.null(df1))

})


test_that("check messages", {

  msg <- capture_output(query_xenocanto(term = '000', verbose = TRUE))

  expect_true(grepl("not found", msg))

  msg <- capture_output(query_xenocanto(term = '000', verbose = FALSE))

  expect_true(msg == "")

  msg <- capture_output(query_xenocanto(term = 'Phaethornis anthophilus', verbose = TRUE, pb = FALSE))

  expect_true(msg == "")


  })
