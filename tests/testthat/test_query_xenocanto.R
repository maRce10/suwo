test_that("search Phaethornis check rows", {

  df1 <- query_xenocanto(term = 'Phaethornis anthophilus')

  expect_true(nrow(df1) > 10)

  })


test_that("search Phaethornis check cols", {

  df1 <- query_xenocanto(term = 'Phaethornis anthophilus', download = FALSE)

  expect_true(ncol(df1) >  20)

})


test_that("no recs found", {

  df1 <- query_xenocanto(term = '000', download = FALSE)

  expect_true(is.null(df1))

})


test_that("check messages", {

  msg <- capture_output(query_xenocanto(term = '000', download = FALSE, verbose = TRUE))

  expect_true(msg == "No recordings were found")

  msg <- capture_output(query_xenocanto(term = '000', download = FALSE, verbose = FALSE))

  expect_true(msg == "")

  msg <- capture_output(query_xenocanto(term = 'Phaethornis anthophilus', download = FALSE, verbose = TRUE, pb = FALSE))

  expect_true(msg == "")


  })
