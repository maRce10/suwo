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

test_that("paging by date", {

  skip_if_not(interactive())
  skip_on_cran()
  skip_if_offline()

  df1 <- query_macaulay(term = 'Calypte costae', format = "image", path = tempdir(), dates = c(2022, 2024, 2025))

  expect_true(nrow(df1) >= 15000)

})
