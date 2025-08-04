test_that("search Glaucis dohrnii sound", {

  skip_if_not(interactive())
  skip_on_cran()
  skip_if_offline()

  df1 <- query_macaulay(term = 'Glaucis dohrnii', type =  "sound", path = tempdir())

  expect_true(nrow(df1) >= 25)

})


test_that("download", {

  skip_if_not(interactive())
  skip_on_cran()
  skip_if_offline()

  df1 <- query_macaulay(term = 'Glaucis dohrnii', type =  "sound", path = tempdir())

  download_media(metadata = df1, path = tempdir())

  expect_true(nrow(df1) >= 25)

})

