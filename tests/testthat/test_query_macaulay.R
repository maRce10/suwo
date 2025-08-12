test_that("search Glaucis dohrnii sound", {

  skip_if_not(interactive())
  skip_on_cran()
  skip_if_offline()

  df1 <- query_macaulay(term = 'Glaucis dohrnii', format =  "sound", path = tempdir())

  expect_true(nrow(df1) >= 12)

})


test_that("search Glaucis dohrnii images", {

  skip_if_not(interactive())
  skip_on_cran()
  skip_if_offline()

  df1 <- query_macaulay(term = 'Glaucis dohrnii', format =  "image", path = tempdir())

  expect_true(nrow(df1) >= 10)

})

